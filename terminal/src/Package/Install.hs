{-# LANGUAGE OverloadedStrings #-}

module Package.Install
  ( Args (..),
    Flags (..),
    run,
  )
where

import BackgroundWriter qualified as BW
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Map.Merge.Strict qualified as Map
import Deps.Package qualified as DPkg
import Deps.Solver qualified as Solver
import Directories qualified as Dirs
import Gren.Constraint qualified as C
import Gren.Details qualified as Details
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import Reporting qualified
import Reporting.Doc ((<+>))
import Reporting.Doc qualified as D
import Reporting.Exit qualified as Exit
import Reporting.Task qualified as Task

-- RUN

data Args
  = NoArgs
  | Install Pkg.Name

data Flags = Flags
  {_skipPrompts :: Bool}

run :: Args -> Flags -> IO ()
run args (Flags _skipPrompts) =
  Reporting.attempt Exit.installToReport $
    do
      maybeRoot <- Dirs.findRoot
      case maybeRoot of
        Nothing ->
          return (Left Exit.InstallNoOutline)
        Just root ->
          Task.run $
            case args of
              NoArgs ->
                Task.eio Exit.InstallBadDetails $ installDependencies root
              Install pkg ->
                do
                  env <- Task.io Solver.initEnv
                  oldOutline <- Task.eio Exit.InstallBadOutline $ Outline.read root
                  case oldOutline of
                    Outline.App outline ->
                      do
                        changes <- makeAppPlan env pkg outline
                        attemptChanges root env _skipPrompts oldOutline V.toChars changes
                    Outline.Pkg outline ->
                      do
                        changes <- makePkgPlan env pkg outline
                        attemptChanges root env _skipPrompts oldOutline C.toChars changes

-- ATTEMPT CHANGES

data Changes vsn
  = AlreadyInstalled
  | PromoteIndirect Outline.Outline
  | Changes (Map.Map Pkg.Name (Change vsn)) Outline.Outline

type Task = Task.Task Exit.Install

attemptChanges :: FilePath -> Solver.Env -> Bool -> Outline.Outline -> (a -> String) -> Changes a -> Task ()
attemptChanges root env skipPrompt oldOutline toChars changes =
  case changes of
    AlreadyInstalled ->
      Task.io $ putStrLn "It is already installed!"
    PromoteIndirect newOutline ->
      attemptChangesHelp root env skipPrompt oldOutline newOutline $
        D.vcat
          [ D.fillSep
              [ "I",
                "found",
                "it",
                "in",
                "your",
                "gren.json",
                "file,",
                "but",
                "in",
                "the",
                D.dullyellow "\"indirect\"",
                "dependencies."
              ],
            D.fillSep
              [ "Should",
                "I",
                "move",
                "it",
                "into",
                D.green "\"direct\"",
                "dependencies",
                "for",
                "more",
                "general",
                "use?",
                "[Y/n]: "
              ]
          ]
    Changes changeDict newOutline ->
      let widths = Map.foldrWithKey (widen toChars) (Widths 0 0 0) changeDict
          changeDocs = Map.foldrWithKey (addChange toChars widths) (Docs [] [] []) changeDict
       in attemptChangesHelp root env skipPrompt oldOutline newOutline $
            D.vcat $
              [ "Here is my plan:",
                viewChangeDocs changeDocs,
                "",
                "Would you like me to update your gren.json accordingly? [Y/n]: "
              ]

attemptChangesHelp :: FilePath -> Solver.Env -> Bool -> Outline.Outline -> Outline.Outline -> D.Doc -> Task ()
attemptChangesHelp root env skipPrompt oldOutline newOutline question =
  Task.eio Exit.InstallBadDetails $
    BW.withScope $ \scope ->
      do
        approved <-
          if skipPrompt
            then return True
            else Reporting.ask question
        if approved
          then do
            Outline.write root newOutline
            result <- Details.verifyInstall scope root env newOutline
            case result of
              Left exit ->
                do
                  Outline.write root oldOutline
                  return (Left exit)
              Right () ->
                do
                  putStrLn "Success!"
                  return (Right ())
          else do
            putStrLn "Okay, I did not change anything!"
            return (Right ())

-- INSTALL DEPENDENCIES

installDependencies :: FilePath -> IO (Either Exit.Details ())
installDependencies path =
  do
    terminalStyle <- Reporting.terminal
    result <- BW.withScope $ \scope ->
      Details.load terminalStyle scope path
    case result of
      Left err ->
        return $ Left err
      Right _ ->
        return $ Right ()

-- MAKE APP PLAN

makeAppPlan :: Solver.Env -> Pkg.Name -> Outline.AppOutline -> Task (Changes V.Version)
makeAppPlan (Solver.Env cache) pkg outline@(Outline.AppOutline _ _ _ direct indirect) =
  if Map.member pkg direct
    then return AlreadyInstalled
    else case Map.lookup pkg indirect of
      Just vsn ->
        return $
          PromoteIndirect $
            Outline.App $
              outline
                { Outline._app_deps_direct = Map.insert pkg vsn direct,
                  Outline._app_deps_indirect = Map.delete pkg indirect
                }
      Nothing -> do
        compatibleVersionResult <-
          Task.io $
            Dirs.withRegistryLock cache $
              DPkg.latestCompatibleVersion cache pkg
        case compatibleVersionResult of
          Left DPkg.NoCompatiblePackage ->
            Task.throw $ Exit.InstallNoCompatiblePkg pkg
          Left (DPkg.GitError gitError) ->
            Task.throw $
              Exit.InstallHadSolverTrouble $
                Exit.SolverBadGitOperationUnversionedPkg pkg gitError
          Right compatibleVersion -> do
            result <- Task.io $ Solver.addToApp Reporting.ignorer cache pkg compatibleVersion outline
            case result of
              Solver.Ok (Solver.AppSolution old new app) ->
                return (Changes (detectChanges old new) (Outline.App app))
              Solver.NoSolution ->
                Task.throw (Exit.InstallNoOnlineAppSolution pkg)
              Solver.Err exit ->
                Task.throw (Exit.InstallHadSolverTrouble exit)

-- MAKE PACKAGE PLAN

makePkgPlan :: Solver.Env -> Pkg.Name -> Outline.PkgOutline -> Task (Changes C.Constraint)
makePkgPlan (Solver.Env cache) pkg outline@(Outline.PkgOutline _ _ _ _ _ deps _ rootPlatform) =
  if Map.member pkg deps
    then return AlreadyInstalled
    else do
      compatibleVersionResult <-
        Task.io $
          Dirs.withRegistryLock cache $
            DPkg.latestCompatibleVersion cache pkg
      case compatibleVersionResult of
        Left DPkg.NoCompatiblePackage ->
          Task.throw $ Exit.InstallNoCompatiblePkg pkg
        Left (DPkg.GitError gitError) ->
          Task.throw $
            Exit.InstallHadSolverTrouble $
              Exit.SolverBadGitOperationUnversionedPkg pkg gitError
        Right compatibleVersion -> do
          let old = deps
          let cons = Map.insert pkg (C.untilNextMajor compatibleVersion) old
          result <- Task.io $ Solver.verify Reporting.ignorer cache rootPlatform cons
          case result of
            Solver.Ok solution ->
              let (Solver.Details vsn _) = solution ! pkg

                  con = C.untilNextMajor vsn
                  new = Map.insert pkg con old
                  changes = detectChanges old new
                  news = Map.mapMaybe keepNew changes
               in return $
                    Changes changes $
                      Outline.Pkg $
                        outline
                          { Outline._pkg_deps = addNews (Just pkg) news deps
                          }
            Solver.NoSolution ->
              Task.throw $ Exit.InstallNoOnlinePkgSolution pkg
            Solver.Err exit ->
              Task.throw $ Exit.InstallHadSolverTrouble exit

addNews :: Maybe Pkg.Name -> Map.Map Pkg.Name C.Constraint -> Map.Map Pkg.Name C.Constraint -> Map.Map Pkg.Name C.Constraint
addNews pkg new old =
  Map.merge
    Map.preserveMissing
    (Map.mapMaybeMissing (\k c -> if Just k == pkg then Just c else Nothing))
    (Map.zipWithMatched (\_ _ n -> n))
    old
    new

-- CHANGES

data Change a
  = Insert a
  | Change a a
  | Remove a

detectChanges :: (Eq a) => Map.Map Pkg.Name a -> Map.Map Pkg.Name a -> Map.Map Pkg.Name (Change a)
detectChanges old new =
  Map.merge
    (Map.mapMissing (\_ v -> Remove v))
    (Map.mapMissing (\_ v -> Insert v))
    (Map.zipWithMaybeMatched keepChange)
    old
    new

keepChange :: (Eq v) => k -> v -> v -> Maybe (Change v)
keepChange _ old new =
  if old == new
    then Nothing
    else Just (Change old new)

keepNew :: Change a -> Maybe a
keepNew change =
  case change of
    Insert a ->
      Just a
    Change _ a ->
      Just a
    Remove _ ->
      Nothing

-- VIEW CHANGE DOCS

data ChangeDocs = Docs
  { _doc_inserts :: [D.Doc],
    _doc_changes :: [D.Doc],
    _doc_removes :: [D.Doc]
  }

viewChangeDocs :: ChangeDocs -> D.Doc
viewChangeDocs (Docs inserts changes removes) =
  D.indent 2 $
    D.vcat $
      concat $
        [ viewNonZero "Add:" inserts,
          viewNonZero "Change:" changes,
          viewNonZero "Remove:" removes
        ]

viewNonZero :: String -> [D.Doc] -> [D.Doc]
viewNonZero title entries =
  if null entries
    then []
    else
      [ "",
        D.fromChars title,
        D.indent 2 (D.vcat entries)
      ]

-- VIEW CHANGE

addChange :: (a -> String) -> Widths -> Pkg.Name -> Change a -> ChangeDocs -> ChangeDocs
addChange toChars widths name change (Docs inserts changes removes) =
  case change of
    Insert new ->
      Docs (viewInsert toChars widths name new : inserts) changes removes
    Change old new ->
      Docs inserts (viewChange toChars widths name old new : changes) removes
    Remove old ->
      Docs inserts changes (viewRemove toChars widths name old : removes)

viewInsert :: (a -> String) -> Widths -> Pkg.Name -> a -> D.Doc
viewInsert toChars (Widths nameWidth leftWidth _) name new =
  viewName nameWidth name <+> pad leftWidth (toChars new)

viewChange :: (a -> String) -> Widths -> Pkg.Name -> a -> a -> D.Doc
viewChange toChars (Widths nameWidth leftWidth rightWidth) name old new =
  D.hsep
    [ viewName nameWidth name,
      pad leftWidth (toChars old),
      "=>",
      pad rightWidth (toChars new)
    ]

viewRemove :: (a -> String) -> Widths -> Pkg.Name -> a -> D.Doc
viewRemove toChars (Widths nameWidth leftWidth _) name old =
  viewName nameWidth name <+> pad leftWidth (toChars old)

viewName :: Int -> Pkg.Name -> D.Doc
viewName width name =
  D.fill (width + 3) (D.fromPackage name)

pad :: Int -> String -> D.Doc
pad width string =
  D.fromChars (replicate (width - length string) ' ') <> D.fromChars string

-- WIDTHS

data Widths = Widths
  { _name :: !Int,
    _left :: !Int,
    _right :: !Int
  }

widen :: (a -> String) -> Pkg.Name -> Change a -> Widths -> Widths
widen toChars pkg change (Widths name left right) =
  let toLength a =
        length (toChars a)

      newName =
        max name (length (Pkg.toChars pkg))
   in case change of
        Insert new ->
          Widths newName (max left (toLength new)) right
        Change old new ->
          Widths newName (max left (toLength old)) (max right (toLength new))
        Remove old ->
          Widths newName (max left (toLength old)) right
