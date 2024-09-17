{-# LANGUAGE OverloadedStrings #-}

module Package.Validate
  ( run,
  )
where

import BackgroundWriter qualified as BW
import Build qualified
import Control.Monad (void)
import Data.Either qualified as Either
import Data.List qualified as List
import Data.Map qualified as Map
import Data.NonEmptyList qualified as NE
import Deps.Diff qualified as Diff
import Deps.Package qualified as Package
import Directories qualified as Dirs
import File qualified
import Git qualified
import Gren.Constraint qualified as Con
import Gren.Details qualified as Details
import Gren.Docs qualified as Docs
import Gren.Magnitude qualified as M
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Gren.PossibleFilePath (PossibleFilePath)
import Gren.PossibleFilePath qualified as PossibleFilePath
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

run :: IO ()
run =
  Reporting.attempt Exit.validateToReport $
    Task.run $
      validate =<< getEnv

-- ENV

data Env = Env
  { _root :: FilePath,
    _cache :: Dirs.PackageCache,
    _outline :: Outline.Outline
  }

getEnv :: Task.Task Exit.Validate Env
getEnv =
  do
    root <- Task.mio Exit.ValidateNoOutline Dirs.findRoot
    cache <- Task.io Dirs.getPackageCache
    outline <- Task.eio Exit.ValidateBadOutline $ Outline.read root
    return $ Env root cache outline

-- VALIDATE

validate :: Env -> Task.Task Exit.Validate ()
validate env@(Env root _ outline) =
  case outline of
    Outline.App _ ->
      Task.throw Exit.ValidateApplication
    Outline.Pkg (Outline.PkgOutline pkg summary _ vsn exposed deps _ _) ->
      do
        knownVersionsResult <- Task.io $ Package.getVersions pkg
        let knownVersionsMaybe = Either.either (const Nothing) Just knownVersionsResult
        reportValidateStart pkg vsn knownVersionsMaybe

        if noExposed exposed then Task.throw Exit.ValidateNoExposed else return ()
        if badSummary summary then Task.throw Exit.ValidateNoSummary else return ()

        verifyReadme root
        verifyLicense root
        verifyNoLocalDeps deps
        docs <- verifyBuild root
        verifyVersion env pkg vsn docs knownVersionsMaybe
        verifyTag vsn
        verifyNoChanges vsn
        verifyNoUnsignedKernelCode root

        Task.io $ putStrLn "Everything looks good!"

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

verifyReadme :: FilePath -> Task.Task Exit.Validate ()
verifyReadme root =
  reportReadmeCheck $
    do
      let readmePath = root </> "README.md"
      exists <- File.exists readmePath
      if not exists
        then return (Left Exit.ValidateNoReadme)
        else do
          size <- IO.withFile readmePath IO.ReadMode IO.hFileSize
          if size < 300
            then return (Left Exit.ValidateShortReadme)
            else return (Right ())

-- VERIFY LICENSE

verifyLicense :: FilePath -> Task.Task Exit.Validate ()
verifyLicense root =
  reportLicenseCheck $
    do
      let licensePath = root </> "LICENSE"
      exists <- File.exists licensePath
      if exists
        then return (Right ())
        else return (Left Exit.ValidateNoLicense)

-- VERIFY NO LOCAL DEPS

verifyNoLocalDeps :: Map.Map Pkg.Name (PossibleFilePath Con.Constraint) -> Task.Task Exit.Validate ()
verifyNoLocalDeps deps =
  reportLocalDepsCheck $
    do
      if any PossibleFilePath.is (Map.elems deps)
        then return (Left Exit.ValidateHasLocalDependencies)
        else return (Right ())

-- VERIFY BUILD

verifyBuild :: FilePath -> Task.Task Exit.Validate Docs.Documentation
verifyBuild root =
  reportBuildCheck $
    BW.withScope $ \scope ->
      Task.run $
        do
          details@(Details.Details _ outline _ _ _ _) <-
            Task.eio Exit.ValidateBadDetails $
              Details.load Reporting.silent scope root

          exposed <-
            case outline of
              Details.ValidApp _ _ -> Task.throw Exit.ValidateApplication
              Details.ValidPkg _ _ [] -> Task.throw Exit.ValidateNoExposed
              Details.ValidPkg _ _ (e : es) -> return (NE.List e es)

          Task.eio Exit.ValidateBuildProblem $
            Build.fromExposed Reporting.silent root details Build.KeepDocs exposed

-- VERIFY LOCAL TAG

verifyTag :: V.Version -> Task.Task Exit.Validate ()
verifyTag vsn =
  reportTagCheck vsn $
    do
      result <- Git.hasLocalTag vsn
      case result of
        Left Git.MissingGit ->
          return $ Left Exit.ValidateNoGit
        Left _ ->
          return $ Left $ Exit.ValidateMissingTag vsn
        Right () ->
          return $ Right ()

-- VERIFY NO LOCAL CHANGES SINCE TAG

verifyNoChanges :: V.Version -> Task.Task Exit.Validate ()
verifyNoChanges vsn =
  reportLocalChangesCheck $
    do
      result <- Git.hasLocalChangesSinceTag vsn
      case result of
        Left Git.MissingGit ->
          return $ Left Exit.ValidateNoGit
        Left _ ->
          return $ Left $ Exit.ValidateLocalChanges vsn
        Right () ->
          return $ Right ()

-- VERIFY NO UNSIGNED KERNEL CODE

verifyNoUnsignedKernelCode :: FilePath -> Task.Task Exit.Validate ()
verifyNoUnsignedKernelCode path =
  reportUnsignedKernelCodeCheck $
    do
      result <- Git.kernelCodeSignedByLeadDeveloper path
      if result
        then return $ Right ()
        else return $ Left Exit.ValidateUnsignedKernelCode

-- VERIFY VERSION

data GoodVersion
  = GoodStart
  | GoodBump V.Version M.Magnitude

verifyVersion :: Env -> Pkg.Name -> V.Version -> Docs.Documentation -> Maybe (V.Version, [V.Version]) -> Task.Task Exit.Validate ()
verifyVersion env pkg vsn newDocs publishedVersions =
  reportSemverCheck vsn $
    case publishedVersions of
      Nothing ->
        if vsn == V.one
          then return $ Right GoodStart
          else return $ Left $ Exit.ValidateNotInitialVersion vsn
      Just vsns ->
        verifyBump env pkg vsn newDocs vsns

verifyBump :: Env -> Pkg.Name -> V.Version -> Docs.Documentation -> (V.Version, [V.Version]) -> IO (Either Exit.Validate GoodVersion)
verifyBump (Env _ cache _) pkg vsn newDocs knownVersions@(latest, previous) =
  case List.find (\(_, new, _) -> vsn == new) (Package.bumpPossibilities knownVersions) of
    Nothing ->
      case List.find (\known -> vsn == known) (latest : previous) of
        Just _ ->
          return $ Right $ GoodBump vsn M.PATCH
        Nothing ->
          return $
            Left $
              Exit.ValidateInvalidBump vsn latest
    Just (old, new, magnitude) ->
      do
        result <- Task.run $ Diff.getDocs cache pkg old
        case result of
          Left dp ->
            return $ Left $ Exit.ValidateCannotGetDocs old new dp
          Right oldDocs ->
            let changes = Diff.diff oldDocs newDocs
                realNew = Diff.bump changes old
             in if new == realNew
                  then return $ Right $ GoodBump old magnitude
                  else
                    return $
                      Left $
                        Exit.ValidateBadBump old new magnitude realNew (Diff.toMagnitude changes)

-- REPORTING

reportValidateStart :: Pkg.Name -> V.Version -> Maybe (V.Version, [V.Version]) -> Task.Task x ()
reportValidateStart pkg vsn maybeKnownVersions =
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

reportLocalDepsCheck :: IO (Either x a) -> Task.Task x a
reportLocalDepsCheck =
  reportCheck
    "Making sure there are no local dependencies"
    "Found no local dependencies"
    "Problem with dependencies"

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

reportUnsignedKernelCodeCheck :: IO (Either x a) -> Task.Task x a
reportUnsignedKernelCodeCheck =
  reportCheck
    "Checking for unsigned kernel code..."
    "No unsigned kernel code found"
    "Your project contains unsigned kernel code"

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
