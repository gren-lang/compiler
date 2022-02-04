{-# LANGUAGE OverloadedStrings #-}
module Publish
  ( run
  )
  where


import Control.Monad (void)
import qualified Data.List as List
import qualified Data.Either as Either
import qualified Data.NonEmptyList as NE
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.Info as Info
import qualified System.IO as IO
import qualified System.Process as Process

import qualified BackgroundWriter as BW
import qualified Build
import qualified Deps.Package as Package
import qualified Deps.Diff as Diff
import qualified Elm.Details as Details
import qualified Elm.Docs as Docs
import qualified Elm.Magnitude as M
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Json.String as Json
import qualified Reporting
import Reporting.Doc ((<+>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Task as Task
import qualified Directories as Dirs



-- RUN


run :: () -> () -> IO ()
run () () =
  Reporting.attempt Exit.publishToReport $
    Task.run $ publish =<< getEnv



-- ENV


data Env =
  Env
    { _root :: FilePath
    , _cache :: Dirs.PackageCache
    , _outline :: Outline.Outline
    }


getEnv :: Task.Task Exit.Publish Env
getEnv =
  do  root <- Task.mio Exit.PublishNoOutline Dirs.findRoot
      cache <- Task.io Dirs.getPackageCache
      outline <- Task.eio Exit.PublishBadOutline $ Outline.read root
      return $ Env root cache outline



-- PUBLISH


publish ::  Env -> Task.Task Exit.Publish ()
publish env@(Env root cache outline) =
  case outline of
    Outline.App _ ->
      Task.throw Exit.PublishApplication

    Outline.Pkg (Outline.PkgOutline pkg summary _ vsn exposed _ _ _) ->
      do  knownVersionsResult <- Task.io $ Dirs.withRegistryLock cache $ Package.getVersions cache pkg
          let knownVersionsMaybe = Either.either (const Nothing) Just knownVersionsResult
          reportPublishStart pkg vsn knownVersionsMaybe

          if noExposed  exposed then Task.throw Exit.PublishNoExposed else return ()
          if badSummary summary then Task.throw Exit.PublishNoSummary else return ()

          verifyReadme root
          verifyLicense root
          docs <- verifyBuild root
          verifyVersion env pkg vsn docs knownVersionsMaybe
          git <- getGit
          commitHash <- verifyTag git pkg vsn
          verifyNoChanges git commitHash vsn

          Task.io $ putStrLn "Success!"



-- VERIFY SUMMARY


badSummary :: Json.String -> Bool
badSummary summary =
  Json.isEmpty summary || Outline.defaultSummary == summary


noExposed :: Outline.Exposed -> Bool
noExposed exposed =
  case exposed of
    Outline.ExposedList modules ->
      null modules

    Outline.ExposedDict chunks ->
      all (null . snd) chunks



-- VERIFY README


verifyReadme :: FilePath -> Task.Task Exit.Publish ()
verifyReadme root =
  reportReadmeCheck $
  do  let readmePath = root </> "README.md"
      exists <- File.exists readmePath
      if not exists
        then return (Left Exit.PublishNoReadme)
        else
          do  size <- IO.withFile readmePath IO.ReadMode IO.hFileSize
              if size < 300
                then return (Left Exit.PublishShortReadme)
                else return (Right ())



-- VERIFY LICENSE


verifyLicense :: FilePath -> Task.Task Exit.Publish ()
verifyLicense root =
  reportLicenseCheck $
  do  let licensePath = root </> "LICENSE"
      exists <- File.exists licensePath
      if exists
        then return (Right ())
        else return (Left Exit.PublishNoLicense)



-- VERIFY BUILD


verifyBuild :: FilePath -> Task.Task Exit.Publish Docs.Documentation
verifyBuild root =
  reportBuildCheck $ BW.withScope $ \scope ->
    Task.run $
    do  details@(Details.Details _ outline _ _ _ _) <-
          Task.eio Exit.PublishBadDetails $
            Details.load Reporting.silent scope root

        exposed <-
          case outline of
            Details.ValidApp _          -> Task.throw Exit.PublishApplication
            Details.ValidPkg _ []     _ -> Task.throw Exit.PublishNoExposed
            Details.ValidPkg _ (e:es) _ -> return (NE.List e es)

        Task.eio Exit.PublishBuildProblem $
          Build.fromExposed Reporting.silent root details Build.KeepDocs exposed


-- GET GIT
-- TODO: Move to Git module

newtype Git =
  Git { _run :: [String] -> IO Exit.ExitCode }


getGit :: Task.Task Exit.Publish Git
getGit =
  do  maybeGit <- Task.io $ Dir.findExecutable "git"
      case maybeGit of
        Nothing ->
          Task.throw Exit.PublishNoGit

        Just git ->
          return $ Git $ \args ->
            let
              process =
                (Process.proc git args)
                  { Process.std_in  = Process.CreatePipe
                  , Process.std_out = Process.CreatePipe
                  , Process.std_err = Process.CreatePipe
                  }
            in
            Process.withCreateProcess process $ \_ _ _ handle ->
              Process.waitForProcess handle



-- VERIFY GITHUB TAG


verifyTag :: Git -> Pkg.Name -> V.Version -> Task.Task Exit.Publish String
verifyTag git _ vsn =
  -- TODO: Check that tag exist in cached repo, that should mean it's available remote as well
  reportTagCheck vsn $
  do  -- https://stackoverflow.com/questions/1064499/how-to-list-all-git-tags
      exitCode <- _run git [ "show", "--name-only", V.toChars vsn, "--" ]
      case exitCode of
        Exit.ExitFailure _ ->
          return $ Left (Exit.PublishMissingTag vsn)

        Exit.ExitSuccess ->
            return $ Right ""



-- VERIFY NO LOCAL CHANGES SINCE TAG


verifyNoChanges :: Git -> String -> V.Version -> Task.Task Exit.Publish ()
verifyNoChanges git commitHash vsn =
  -- TODO: This doesn't actually compare against a specific tag as far as I can see
  reportLocalChangesCheck $
  do  -- https://stackoverflow.com/questions/3878624/how-do-i-programmatically-determine-if-there-are-uncommited-changes
      exitCode <- _run git [ "diff-index", "--quiet", commitHash, "--" ]
      case exitCode of
        Exit.ExitSuccess   -> return $ Right ()
        Exit.ExitFailure _ -> return $ Left (Exit.PublishLocalChanges vsn)



-- VERIFY VERSION


data GoodVersion
  = GoodStart
  | GoodBump V.Version M.Magnitude


verifyVersion :: Env -> Pkg.Name -> V.Version -> Docs.Documentation -> Maybe (V.Version, [ V.Version ]) -> Task.Task Exit.Publish ()
verifyVersion env pkg vsn newDocs publishedVersions =
  reportSemverCheck vsn $
    case publishedVersions of
      Nothing ->
        if vsn == V.one
        then return $ Right GoodStart
        else return $ Left $ Exit.PublishNotInitialVersion vsn

      Just vsns@(latest, previous) ->
        if vsn == latest || elem vsn previous
          then return $ Left $ Exit.PublishAlreadyPublished vsn
          else verifyBump env pkg vsn newDocs vsns


verifyBump :: Env -> Pkg.Name -> V.Version -> Docs.Documentation -> (V.Version, [ V.Version ]) -> IO (Either Exit.Publish GoodVersion)
verifyBump (Env _ cache _) pkg vsn newDocs knownVersions@(latest, _) =
  case List.find (\(_ ,new, _) -> vsn == new) (Package.bumpPossibilities knownVersions) of
    Nothing ->
      return $ Left $
        Exit.PublishInvalidBump vsn latest

    Just (old, new, magnitude) ->
        do  result <- Task.run $ Diff.getDocs cache pkg old
            case result of
              Left dp ->
                 return $ Left $ Exit.PublishCannotGetDocs old new dp

              Right oldDocs ->
                  let
                    changes = Diff.diff oldDocs newDocs
                    realNew = Diff.bump changes old
                  in
                  if new == realNew
                     then return $ Right $ GoodBump old magnitude
                  else
                    return $ Left $
                        Exit.PublishBadBump old new magnitude realNew (Diff.toMagnitude changes)



-- REPORTING


reportPublishStart :: Pkg.Name -> V.Version -> Maybe (V.Version, [ V.Version ]) -> Task.Task x ()
reportPublishStart pkg vsn maybeKnownVersions =
  Task.io $
  case maybeKnownVersions of
    Nothing ->
      putStrLn $ Exit.newPackageOverview ++ "\nI will now verify that everything is in order...\n"

    Just _ ->
      putStrLn $ "Verifying " ++ Pkg.toChars pkg ++ " " ++ V.toChars vsn ++ " ...\n"



-- REPORTING PHASES


reportReadmeCheck :: IO (Either x a) -> Task.Task x a
reportReadmeCheck =
  reportCheck
    "Looking for README.md"
    "Found README.md"
    "Problem with your README.md"


reportLicenseCheck :: IO (Either x a) -> Task.Task x a
reportLicenseCheck =
  reportCheck
    "Looking for LICENSE"
    "Found LICENSE"
    "Problem with your LICENSE"


reportBuildCheck :: IO (Either x a) -> Task.Task x a
reportBuildCheck =
  reportCheck
    "Verifying documentation..."
    "Verified documentation"
    "Problem with documentation"


reportSemverCheck :: V.Version -> IO (Either x GoodVersion) -> Task.Task x ()
reportSemverCheck version work =
  let
    vsn = V.toChars version

    waiting = "Checking semantic versioning rules. Is " ++ vsn ++ " correct?"
    failure = "Version " ++ vsn ++ " is not correct!"
    success result =
      case result of
        GoodStart ->
          "All packages start at version " ++ V.toChars V.one

        GoodBump oldVersion magnitude ->
          "Version number " ++ vsn ++ " verified ("
          ++ M.toChars magnitude ++ " change, "
          ++ V.toChars oldVersion ++ " => " ++ vsn ++ ")"
  in
  void $ reportCustomCheck waiting success failure work


reportTagCheck :: V.Version -> IO (Either x a) -> Task.Task x a
reportTagCheck vsn =
  reportCheck
    ("Is version " ++ V.toChars vsn ++ " tagged on GitHub?")
    ("Version " ++ V.toChars vsn ++ " is tagged on GitHub")
    ("Version " ++ V.toChars vsn ++ " is not tagged on GitHub!")


reportLocalChangesCheck :: IO (Either x a) -> Task.Task x a
reportLocalChangesCheck =
  reportCheck
    "Checking for uncommitted changes..."
    "No uncommitted changes in local code"
    "Your local code is different than the code tagged on GitHub"


reportCheck :: String -> String -> String -> IO (Either x a) -> Task.Task x a
reportCheck waiting success failure work =
  reportCustomCheck waiting (\_ -> success) failure work


reportCustomCheck :: String -> (a -> String) -> String -> IO (Either x a) -> Task.Task x a
reportCustomCheck waiting success failure work =
  let
    putFlush doc =
      Help.toStdout doc >> IO.hFlush IO.stdout

    padded message =
      message ++ replicate (length waiting - length message) ' '
  in
  Task.eio id $
  do  putFlush $ "  " <> waitingMark <+> D.fromChars waiting
      result <- work
      putFlush $
        case result of
          Right a -> "\r  " <> goodMark <+> D.fromChars (padded (success a) ++ "\n")
          Left _  -> "\r  " <> badMark  <+> D.fromChars (padded failure ++ "\n\n")

      return result


-- MARKS


goodMark :: D.Doc
goodMark =
  D.green $ if isWindows then "+" else "●"


badMark :: D.Doc
badMark =
  D.red $ if isWindows then "X" else "✗"


waitingMark :: D.Doc
waitingMark =
  D.dullyellow $ if isWindows then "-" else "→"


isWindows :: Bool
isWindows =
  Info.os == "mingw32"
