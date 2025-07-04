{-# LANGUAGE OverloadedStrings #-}

module Package.Validate
  ( Flags (..),
    run,
  )
where

import Build qualified
import Command qualified
import Data.ByteString.Internal (ByteString)
import Data.List qualified as List
import Data.Map (Map)
import Data.NonEmptyList qualified as NE
import Deps.Diff qualified as Diff
import Deps.Package qualified as Pkg
import Gren.Details qualified as Details
import Gren.Docs qualified as Docs
import Gren.Magnitude qualified as M
import Gren.ModuleName qualified as ModuleName
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import Reporting qualified
import Reporting.Doc ((<+>))
import Reporting.Doc qualified as D
import Reporting.Exit qualified as Exit
import Reporting.Exit.Help qualified as Help
import Reporting.Task qualified as Task
import System.IO qualified as IO
import System.Info qualified as Info

data Flags = Flags
  { _project_path :: String,
    _known_versions :: [V.Version],
    _current_version :: Command.ProjectInfo,
    _previous_version :: Maybe Command.ProjectInfo
  }
  deriving (Show)

run :: Flags -> IO ()
run flags =
  Reporting.attempt Exit.validateToReport $
    Task.run $
      validate flags

validate :: Flags -> Task.Task Exit.Validate ()
validate (Flags root knownVersions (Command.ProjectInfo currentOutline currentSources currentDeps) maybePreviousVersion) =
  case (currentOutline, maybePreviousVersion) of
    (Outline.App _, _) ->
      Task.throw Exit.ValidateApplication
    (Outline.Pkg currentPkgOutline@(Outline.PkgOutline _ _ _ _ _ _ _ _), Nothing) ->
      do
        _ <- verifyBuild root currentPkgOutline currentSources currentDeps
        Task.io $ putStrLn "Everything looks good!"
    (Outline.Pkg (Outline.PkgOutline _ _ _ _ _ _ _ _), Just (Command.ProjectInfo (Outline.App _) _ _)) ->
      error "Previous version is app"
    (Outline.Pkg currentPkgOutline@(Outline.PkgOutline _ _ _ vsn _ _ _ _), Just (Command.ProjectInfo (Outline.Pkg previousOutline) previousSources previousDeps)) ->
      do
        currentDocs <- verifyBuild root currentPkgOutline currentSources currentDeps
        previousDocs <- buildProject root previousOutline previousSources previousDeps

        _ <- Task.eio id $ verifyBump vsn currentDocs previousDocs knownVersions

        Task.io $ putStrLn "Everything looks good!"

verifyBuild :: FilePath -> Outline.PkgOutline -> Map ModuleName.Raw ByteString -> Map Pkg.Name Details.Dependency -> Task.Task Exit.Validate Docs.Documentation
verifyBuild root outline sources solution =
  reportBuildCheck $
    Task.run $
      buildProject root outline sources solution

buildProject :: FilePath -> Outline.PkgOutline -> Map ModuleName.Raw ByteString -> Map Pkg.Name Details.Dependency -> Task.Task Exit.Validate Docs.Documentation
buildProject root pkgOutline@(Outline.PkgOutline _ _ _ _ _ _ _ _) sources solution =
  do
    details@(Details.Details _ outline _ _ _ _) <-
      Task.eio Exit.ValidateBadDetails $
        Details.load (Outline.Pkg pkgOutline) solution

    exposed <-
      case outline of
        Details.ValidApp _ _ -> Task.throw Exit.ValidateApplication
        Details.ValidPkg _ _ [] -> Task.throw Exit.ValidateNoExposed
        Details.ValidPkg _ _ (e : es) -> return (NE.List e es)

    Task.eio Exit.ValidateBuildProblem $
      Build.fromExposed Reporting.silent root details sources Build.KeepDocs exposed

data GoodVersion
  = GoodBump V.Version M.Magnitude

verifyBump :: V.Version -> Docs.Documentation -> Docs.Documentation -> [V.Version] -> IO (Either Exit.Validate GoodVersion)
verifyBump vsn newDocs oldDocs knownVersions =
  case reverse knownVersions of
    [] ->
      error "Known versions field was empty"
    latest : previous ->
      case List.find (\(_, new, _) -> vsn == new) (Pkg.bumpPossibilities (latest, previous)) of
        Nothing ->
          case List.find (\known -> vsn == known) (latest : previous) of
            Just _ ->
              return $ Right $ GoodBump vsn M.PATCH
            Nothing ->
              return $
                Left $
                  Exit.ValidateInvalidBump vsn latest
        Just (old, new, magnitude) ->
          let changes = Diff.diff oldDocs newDocs
              realNew = Diff.bump changes old
           in if new == realNew
                then return $ Right $ GoodBump old magnitude
                else
                  return $
                    Left $
                      Exit.ValidateBadBump old new magnitude realNew (Diff.toMagnitude changes)

-- REPORTING PHASES

reportBuildCheck :: IO (Either x a) -> Task.Task x a
reportBuildCheck =
  reportCheck
    "Verifying documentation..."
    "Verified documentation"
    "Problem with documentation"

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
