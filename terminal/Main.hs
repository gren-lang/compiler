module Main
  ( main,
  )
where

import Command qualified
import Data.ByteString.Char8 qualified
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
        json <- Data.ByteString.Char8.getLine
        case Json.fromByteString Command.commandDecoder json of
          Left err ->
            error (show err)
          Right (Command.Repl (Command.ReplFlags interpreter root outline rootSources deps)) ->
            Repl.run $ Repl.Flags interpreter root outline rootSources deps
          Right (Command.Make (Command.MakeFlags optimize sourcemaps output report paths projectPath outline rootSources deps)) ->
            Make.run $ Make.Flags optimize sourcemaps output report paths projectPath outline rootSources deps
          Right (Command.Docs (Command.DocsFlags output report projectPath outline rootSources deps)) ->
            Docs.run $ Docs.Flags output report projectPath outline rootSources deps
          Right (Command.PackageValidate (Command.ValidateFlags projectPath knownVersions currentVersion maybePreviousVersion)) ->
            Validate.run $ Validate.Flags projectPath knownVersions currentVersion maybePreviousVersion
          Right (Command.PackageBump (Command.BumpFlags interactive projectPath knownVersions currentVersion publishedVersion)) ->
            Bump.run $ Bump.Flags interactive projectPath knownVersions currentVersion publishedVersion
          Right (Command.PackageDiff (Command.DiffFlags interactive projectPath firstPackage secondPackage)) ->
            Diff.run $ Diff.Flags interactive projectPath firstPackage secondPackage
      _ ->
        do
          putStrLn "Expected exactly 0 arguments."
          putStrLn ""
          putStrLn
            "It looks like you are trying to run Gren's internal backend directly.\
            \ To properly install Gren, see https://gren-lang.org/install"
