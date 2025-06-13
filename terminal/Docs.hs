{-# LANGUAGE OverloadedStrings #-}

module Docs
  ( Flags (..),
    Output (..),
    run,
  )
where

import Build qualified
import Data.ByteString.Builder qualified as B
import Data.ByteString.Internal (ByteString)
import Data.Map (Map)
import Data.NonEmptyList qualified as NE
import Gren.Details qualified as Details
import Gren.Docs qualified as Docs
import Gren.ModuleName qualified as ModuleName
import Gren.Outline (Outline)
import Gren.Package qualified as Package
import Json.Encode qualified as Json
import Reporting qualified
import Reporting.Exit qualified as Exit
import Reporting.Task qualified as Task
import System.IO qualified as IO

-- FLAGS

data Flags = Flags
  { _output :: Maybe Output,
    _report :: Bool,
    _project_root :: FilePath,
    _outline :: Outline,
    _root_sources :: Map ModuleName.Raw ByteString,
    _dependencies :: Map Package.Name Details.Dependency
  }

data Output
  = JSON FilePath
  | DevNull
  | DevStdOut
  deriving (Show)

-- RUN

type Task a = Task.Task Exit.Docs a

run :: Flags -> IO ()
run flags@(Flags _ report _ _ _ _) =
  do
    style <- getStyle report
    Reporting.attemptWithStyle style Exit.docsToReport $
      runHelp style flags

runHelp :: Reporting.Style -> Flags -> IO (Either Exit.Docs ())
runHelp style (Flags maybeOutput _ root outline sources dependencies) =
  Task.run $
    do
      details <- Task.eio Exit.DocsBadDetails (Details.load style outline dependencies)
      exposed <- getExposed details
      case maybeOutput of
        Just DevNull ->
          do
            _ <- buildExposed style root details sources Build.KeepDocs exposed
            return ()
        Just DevStdOut ->
          do
            docs <- buildExposed Reporting.silent root details sources Build.KeepDocs exposed
            let builder = Json.encodeUgly $ Docs.encode docs
            Task.io $ B.hPutBuilder IO.stdout builder
        Nothing ->
          buildExposed style root details sources (Build.WriteDocs "docs.json") exposed
        Just (JSON target) ->
          buildExposed style root details sources (Build.WriteDocs target) exposed

-- GET INFORMATION

getStyle :: Bool -> IO Reporting.Style
getStyle report =
  if report then return Reporting.json else Reporting.terminal

getExposed :: Details.Details -> Task (NE.List ModuleName.Raw)
getExposed (Details.Details _ validOutline _ _ _ _) =
  case validOutline of
    Details.ValidApp _ _ ->
      Task.throw Exit.DocsApplication
    Details.ValidPkg _ _ exposed ->
      case exposed of
        [] -> Task.throw Exit.DocsNoExposed
        m : ms -> return (NE.List m ms)

-- BUILD PROJECTS

buildExposed :: Reporting.Style -> FilePath -> Details.Details -> Map ModuleName.Raw ByteString -> Build.DocsGoal a -> NE.List ModuleName.Raw -> Task a
buildExposed style root details sources docsGoal exposed =
  Task.eio Exit.DocsBadBuild $
    Build.fromExposed style root details sources docsGoal exposed
