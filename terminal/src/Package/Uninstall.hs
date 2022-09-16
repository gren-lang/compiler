{-# LANGUAGE OverloadedStrings #-}

module Package.Uninstall
  ( Args (..),
    Flags (..),
    run,
  )
where

import BackgroundWriter qualified as BW
import Data.Map qualified as Map
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
  = Uninstall Pkg.Name

data Flags = Flags
  { _skipPrompts :: Bool }

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
            do
              env <- Task.io Solver.initEnv
              oldOutline <- Task.eio Exit.InstallBadOutline $ Outline.read root
              case args of
                Uninstall pkg ->
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
  = NoSuchPackage
  | MakeIndirect Outline.Outline
  | Changes (Map.Map Pkg.Name vsn) Outline.Outline

type Task = Task.Task Exit.Install

attemptChanges :: FilePath -> Solver.Env -> Bool -> Outline.Outline -> (a -> String) -> Changes a -> Task ()
attemptChanges root env skipPrompt oldOutline toChars changes =
  case changes of
    NoSuchPackage ->
      Task.io $ putStrLn "This package doesn't exist in your project."
    MakeIndirect newOutline ->
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
          changeDocs = Map.foldrWithKey (addChange toChars widths) ([]) changeDict
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
        approved <- if skipPrompt
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

-- MAKE APP PLAN

makeAppPlan :: Solver.Env -> Pkg.Name -> Outline.AppOutline -> Task (Changes V.Version)
makeAppPlan (Solver.Env cache) pkg outline@(Outline.AppOutline _ rootPlatform _ direct indirect) =
  case Map.lookup pkg direct of
    Just vsn -> do
      let constraints = toConstraints direct indirect
      let withMissingPkg = Map.delete pkg constraints
      result <- Task.io $ Solver.verify cache rootPlatform withMissingPkg
      case result of
        Solver.Ok solution ->
          let old = Map.union direct indirect
              new = Map.map (\(Solver.Details v _) -> v) solution
           in if Map.member pkg new
                then
                  return $
                    MakeIndirect $
                      Outline.App $
                        outline
                          { Outline._app_deps_direct = Map.delete pkg direct,
                            Outline._app_deps_indirect = Map.insert pkg vsn indirect
                          }
                else
                  return $
                    Changes (Map.difference old new) $
                      Outline.App $
                        outline
                          { Outline._app_deps_direct = Map.intersection direct new,
                            Outline._app_deps_indirect = Map.intersection indirect new
                          }
        Solver.NoSolution ->
          Task.throw $ Exit.InstallNoOnlinePkgSolution pkg
        Solver.Err exit ->
          Task.throw $ Exit.InstallHadSolverTrouble exit
    Nothing ->
      case Map.lookup pkg indirect of
        Just _ -> do
          let constraints = toConstraints direct indirect
          let withMissingPkg = Map.delete pkg constraints
          result <- Task.io $ Solver.verify cache rootPlatform withMissingPkg
          case result of
            Solver.Ok solution ->
              let old = Map.union direct indirect
                  new = Map.map (\(Solver.Details v _) -> v) solution
               in if Map.member pkg new
                    then -- TODO: Create better error
                      Task.throw $ Exit.InstallNoOnlinePkgSolution pkg
                    else
                      return $
                        Changes (Map.difference old new) $
                          Outline.App $
                            outline
                              { Outline._app_deps_direct = Map.intersection direct new,
                                Outline._app_deps_indirect = Map.intersection indirect new
                              }
            Solver.NoSolution ->
              Task.throw $ Exit.InstallNoOnlinePkgSolution pkg
            Solver.Err exit ->
              Task.throw $ Exit.InstallHadSolverTrouble exit
        Nothing ->
          return NoSuchPackage

toConstraints :: Map.Map Pkg.Name V.Version -> Map.Map Pkg.Name V.Version -> Map.Map Pkg.Name C.Constraint
toConstraints direct indirect =
  Map.map C.exactly $ Map.union direct indirect

-- MAKE PACKAGE PLAN

makePkgPlan :: Solver.Env -> Pkg.Name -> Outline.PkgOutline -> Task (Changes C.Constraint)
makePkgPlan (Solver.Env cache) pkg outline@(Outline.PkgOutline _ _ _ _ _ deps _ rootPlatform) =
  if not $ Map.member pkg deps
    then return NoSuchPackage
    else do
      let withMissingPkg = Map.delete pkg deps
      result <- Task.io $ Solver.verify cache rootPlatform withMissingPkg
      case result of
        Solver.Ok _ ->
          let changes = Map.difference deps withMissingPkg
           in return $
                Changes changes $
                  Outline.Pkg $
                    outline
                      { Outline._pkg_deps = withMissingPkg
                      }
        Solver.NoSolution ->
          Task.throw $ Exit.InstallNoOnlinePkgSolution pkg
        Solver.Err exit ->
          Task.throw $ Exit.InstallHadSolverTrouble exit

-- VIEW CHANGE DOCS

type ChangeDocs = [D.Doc]

viewChangeDocs :: ChangeDocs -> D.Doc
viewChangeDocs removes =
  D.indent 2 $
    D.vcat $
      viewNonZero "Remove:" removes

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

addChange :: (a -> String) -> Widths -> Pkg.Name -> a -> ChangeDocs -> ChangeDocs
addChange toChars widths name change removes =
  viewRemove toChars widths name change : removes

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

widen :: (a -> String) -> Pkg.Name -> a -> Widths -> Widths
widen toChars pkg change (Widths name left right) =
  let toLength a =
        length (toChars a)

      newName =
        max name (length (Pkg.toChars pkg))
   in Widths newName (max left (toLength change)) right
