{-# LANGUAGE OverloadedStrings #-}

module Docs
  ( Flags (..),
    Output (..),
    run,
  )
where

import BackgroundWriter qualified as BW
import Build qualified
import Data.ByteString.Builder qualified as B
import Data.NonEmptyList qualified as NE
import Directories qualified as Dirs
import Gren.Details qualified as Details
import Gren.Docs qualified as Docs
import Gren.ModuleName qualified as ModuleName
import Json.Encode qualified as Json
import Reporting qualified
import Reporting.Exit qualified as Exit
import Reporting.Task qualified as Task
import System.IO qualified as IO

-- FLAGS

data Flags = Flags
  { _output :: Maybe Output,
    _report :: Bool
  }

data Output
  = JSON FilePath
  | DevNull
  | DevStdOut
  deriving (Show)

-- RUN

type Task a = Task.Task Exit.Make a

run :: Flags -> IO ()
run flags@(Flags _ report) =
  do
    style <- getStyle report
    maybeRoot <- Dirs.findRoot
    Reporting.attemptWithStyle style Exit.makeToReport $
      case maybeRoot of
        Just root -> runHelp root style flags
        Nothing -> return $ Left Exit.MakeNoOutline

runHelp :: FilePath -> Reporting.Style -> Flags -> IO (Either Exit.Make ())
runHelp root style (Flags maybeOutput _) =
  BW.withScope $ \scope ->
    Dirs.withRootLock root $
      Task.run $
        do
          details <- Task.eio Exit.MakeBadDetails (Details.load style scope root)
          exposed <- getExposed details
          case maybeOutput of
            Just DevNull ->
              do
                buildExposed style root details Build.IgnoreDocs exposed
                return ()
            Just DevStdOut ->
              do
                docs <- buildExposed Reporting.silent root details Build.KeepDocs exposed
                let builder = Json.encodeUgly $ Docs.encode docs
                Task.io $ B.hPutBuilder IO.stdout builder
            Nothing ->
              buildExposed style root details (Build.WriteDocs "docs.json") exposed
            Just (JSON target) ->
              buildExposed style root details (Build.WriteDocs target) exposed

-- GET INFORMATION

getStyle :: Bool -> IO Reporting.Style
getStyle report =
  if report then return Reporting.json else Reporting.terminal

getExposed :: Details.Details -> Task (NE.List ModuleName.Raw)
getExposed (Details.Details _ validOutline _ _ _ _) =
  case validOutline of
    Details.ValidApp _ _ ->
      Task.throw Exit.MakeAppNeedsFileNames
    Details.ValidPkg _ _ exposed ->
      case exposed of
        [] -> Task.throw Exit.MakePkgNeedsExposing
        m : ms -> return (NE.List m ms)

-- BUILD PROJECTS

buildExposed :: Reporting.Style -> FilePath -> Details.Details -> Build.DocsGoal a -> NE.List ModuleName.Raw -> Task a
buildExposed style root details docsGoal exposed =
  Task.eio Exit.MakeCannotBuild $
    Build.fromExposed style root details docsGoal exposed
