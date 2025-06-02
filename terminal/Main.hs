module Main
  ( main,
  )
where

import Command qualified
import Data.ByteString qualified
import Docs qualified
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Json.Decode qualified as Json
import Make qualified
import Package.Bump qualified as Bump
import Package.Diff qualified as Diff
import Package.Validate qualified as Validate
import Repl qualified
import System.Environment qualified as Env

-- MAIN

main :: IO ()
main =
  do
    setLocaleEncoding utf8
    argStrings <- Env.getArgs
    case argStrings of
      [] -> do
        json <- Data.ByteString.getContents
        case Json.fromByteString Command.commandDecoder json of
          Left err ->
            error (show err)
          Right (Command.Repl interpreter) ->
            Repl.run $ Repl.Flags interpreter
          Right (Command.Make (Command.MakeFlags optimize sourcemaps output report paths projectPath outline rootSources deps)) ->
            Make.run $ Make.Flags optimize sourcemaps output report paths projectPath outline rootSources deps
          Right (Command.Docs (Command.DocsFlags output report projectPath outline rootSources deps)) ->
            Docs.run $ Docs.Flags output report projectPath outline rootSources deps
          Right Command.PackageValidate ->
            Validate.run
          Right (Command.PackageBump (Command.BumpFlags interactive projectPath knownVersions currentVersion publishedVersion)) ->
            Bump.run $ Bump.Flags interactive projectPath knownVersions currentVersion publishedVersion
          Right Command.PackageDiffLatest ->
            Diff.run Diff.CodeVsLatest
          Right (Command.PackageDiffVersion version) ->
            Diff.run $ Diff.CodeVsExactly version
          Right (Command.PackageDiffRange from to) ->
            Diff.run $ Diff.LocalInquiry from to
          Right (Command.PackageDiffGlobal pkg from to) ->
            Diff.run $ Diff.GlobalInquiry pkg from to
      _ ->
        do
          putStrLn "Expected exactly 0 arguments."
          putStrLn ""
          putStrLn
            "It looks like you are trying to run Gren's internal backend directly.\
            \ To properly install Gren, see https://gren-lang.org/install"
