{-# LANGUAGE OverloadedStrings #-}

module Publish
  ( run,
  )
where

import BackgroundWriter qualified as BW
import Build qualified
import Control.Monad (void)
import Data.Either qualified as Either
import Data.List qualified as List
import Data.NonEmptyList qualified as NE
import Deps.Diff qualified as Diff
import Deps.Package qualified as Package
import Directories qualified as Dirs
import File qualified
import Git qualified
import Gren.Details qualified as Details
import Gren.Docs qualified as Docs
import Gren.Magnitude qualified as M
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import Json.String qualified as Json
import Reporting qualified
import Reporting.Doc ((<+>))
import Reporting.Doc qualified as D
import Reporting.Exit qualified as Exit
import Reporting.Exit.Help qualified as Help
import Reporting.Task qualified as Task
import System.FilePath ((</>))
import System.IO qualified as IO
import System.Info qualified as Info

-- RUN

run :: () -> () -> IO ()
run () () =
  Reporting.attempt Exit.publishToReport $
    Task.run $
      publish =<< getEnv

-- ENV

data Env = Env
  { _root :: FilePath,
    _cache :: Dirs.PackageCache,
    _outline :: Outline.Outline
  }

getEnv :: Task.Task Exit.Publish Env
getEnv =
  do
    root <- Task.mio Exit.PublishNoOutline Dirs.findRoot
    cache <- Task.io Dirs.getPackageCache
    outline <- Task.eio Exit.PublishBadOutline $ Outline.read root
    return $ Env root cache outline

-- PUBLISH

publish :: Env -> Task.Task Exit.Publish ()
publish env@(Env root _ outline) =
  case outline of
    Outline.App _ ->
      Task.throw Exit.PublishApplication
    Outline.Pkg (Outline.PkgOutline pkg summary _ vsn exposed _ _ _) ->
      do
        knownVersionsResult <- Task.io $ Package.getVersions pkg
        let knownVersionsMaybe = Either.either (const Nothing) Just knownVersionsResult
        reportPublishStart pkg vsn knownVersionsMaybe

        if noExposed exposed then Task.throw Exit.PublishNoExposed else return ()
        if badSummary summary then Task.throw Exit.PublishNoSummary else return ()

        verifyReadme root
        verifyLicense root
        docs <- verifyBuild root
        verifyVersion env pkg vsn docs knownVersionsMaybe
        verifyTag vsn
        verifyNoChanges vsn

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
    do
      let readmePath = root </> "README.md"
      exists <- File.exists readmePath
      if not exists
        then return (Left Exit.PublishNoReadme)
        else do
          size <- IO.withFile readmePath IO.ReadMode IO.hFileSize
          if size < 300
            then return (Left Exit.PublishShortReadme)
            else return (Right ())

-- VERIFY LICENSE

verifyLicense :: FilePath -> Task.Task Exit.Publish ()
verifyLicense root =
  reportLicenseCheck $
    do
      let licensePath = root </> "LICENSE"
      exists <- File.exists licensePath
      if exists
        then return (Right ())
        else return (Left Exit.PublishNoLicense)

-- VERIFY BUILD

verifyBuild :: FilePath -> Task.Task Exit.Publish Docs.Documentation
verifyBuild root =
  reportBuildCheck $
    BW.withScope $ \scope ->
      Task.run $
        do
          details@(Details.Details _ outline _ _ _ _) <-
            Task.eio Exit.PublishBadDetails $
              Details.load Reporting.silent scope root

          exposed <-
            case outline of
              Details.ValidApp _ -> Task.throw Exit.PublishApplication
              Details.ValidPkg _ [] -> Task.throw Exit.PublishNoExposed
              Details.ValidPkg _ (e : es) -> return (NE.List e es)

          Task.eio Exit.PublishBuildProblem $
            Build.fromExposed Reporting.silent root details Build.KeepDocs exposed

-- VERIFY LOCAL TAG

verifyTag :: V.Version -> Task.Task Exit.Publish ()
verifyTag vsn =
  reportTagCheck vsn $
    do
      result <- Git.hasLocalTag vsn
      case result of
        Left Git.MissingGit ->
          return $ Left Exit.PublishNoGit
        Left _ ->
          return $ Left $ Exit.PublishMissingTag vsn
        Right () ->
          return $ Right ()

-- VERIFY NO LOCAL CHANGES SINCE TAG

verifyNoChanges :: V.Version -> Task.Task Exit.Publish ()
verifyNoChanges vsn =
  reportLocalChangesCheck $
    do
      result <- Git.hasLocalChangesSinceTag vsn
      case result of
        Left Git.MissingGit ->
          return $ Left Exit.PublishNoGit
        Left _ ->
          return $ Left $ Exit.PublishLocalChanges vsn
        Right () ->
          return $ Right ()

-- VERIFY VERSION

data GoodVersion
  = GoodStart
  | GoodBump V.Version M.Magnitude

verifyVersion :: Env -> Pkg.Name -> V.Version -> Docs.Documentation -> Maybe (V.Version, [V.Version]) -> Task.Task Exit.Publish ()
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

verifyBump :: Env -> Pkg.Name -> V.Version -> Docs.Documentation -> (V.Version, [V.Version]) -> IO (Either Exit.Publish GoodVersion)
verifyBump (Env _ cache _) pkg vsn newDocs knownVersions@(latest, _) =
  case List.find (\(_, new, _) -> vsn == new) (Package.bumpPossibilities knownVersions) of
    Nothing ->
      return $
        Left $
          Exit.PublishInvalidBump vsn latest
    Just (old, new, magnitude) ->
      do
        result <- Task.run $ Diff.getDocs cache pkg old
        case result of
          Left dp ->
            return $ Left $ Exit.PublishCannotGetDocs old new dp
          Right oldDocs ->
            let changes = Diff.diff oldDocs newDocs
                realNew = Diff.bump changes old
             in if new == realNew
                  then return $ Right $ GoodBump old magnitude
                  else
                    return $
                      Left $
                        Exit.PublishBadBump old new magnitude realNew (Diff.toMagnitude changes)

-- REPORTING

reportPublishStart :: Pkg.Name -> V.Version -> Maybe (V.Version, [V.Version]) -> Task.Task x ()
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
  let vsn = V.toChars version

      waiting = "Checking semantic versioning rules. Is " ++ vsn ++ " correct?"
      failure = "Version " ++ vsn ++ " is not correct!"
      success result =
        case result of
          GoodStart ->
            "All packages start at version " ++ V.toChars V.one
          GoodBump oldVersion magnitude ->
            "Version number "
              ++ vsn
              ++ " verified ("
              ++ M.toChars magnitude
              ++ " change, "
              ++ V.toChars oldVersion
              ++ " => "
              ++ vsn
              ++ ")"
   in void $ reportCustomCheck waiting success failure work

reportTagCheck :: V.Version -> IO (Either x a) -> Task.Task x a
reportTagCheck vsn =
  reportCheck
    ("Is version " ++ V.toChars vsn ++ " tagged?")
    ("Version " ++ V.toChars vsn ++ " is tagged")
    ("Version " ++ V.toChars vsn ++ " is not tagged!")

reportLocalChangesCheck :: IO (Either x a) -> Task.Task x a
reportLocalChangesCheck =
  reportCheck
    "Checking for uncommitted changes..."
    "No uncommitted changes in local code"
    "Your local code is different than the code tagged in your git repo"

reportCheck :: String -> String -> String -> IO (Either x a) -> Task.Task x a
reportCheck waiting success failure work =
  reportCustomCheck waiting (\_ -> success) failure work

reportCustomCheck :: String -> (a -> String) -> String -> IO (Either x a) -> Task.Task x a
reportCustomCheck waiting success failure work =
  let putFlush doc =
        Help.toStdout doc >> IO.hFlush IO.stdout

      padded message =
        message ++ replicate (length waiting - length message) ' '
   in Task.eio id $
        do
          putFlush $ "  " <> waitingMark <+> D.fromChars waiting
          result <- work
          putFlush $
            case result of
              Right a -> "\r  " <> goodMark <+> D.fromChars (padded (success a) ++ "\n")
              Left _ -> "\r  " <> badMark <+> D.fromChars (padded failure ++ "\n\n")

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
