module Main
  ( main,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified
import Data.ByteString.Char8 qualified as BS
import Data.Map (Map)
import Data.Utf8 qualified as Utf8
import Docs qualified
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Gren.Details qualified as Details
import Gren.ModuleName qualified as ModuleName
import Gren.Outline (Outline)
import Gren.Outline qualified as Outline
import Gren.Package qualified as Package
import Gren.Version qualified as Version
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
        case Json.fromByteString commandDecoder json of
          Left err ->
            error (show err)
          Right (Repl interpreter) ->
            Repl.run $ Repl.Flags interpreter
          Right (Make (MakeFlags optimize sourcemaps output report paths projectPath outline rootSources deps)) ->
            Make.run $ Make.Flags optimize sourcemaps output report paths projectPath outline rootSources deps
          Right (Docs (DocsFlags output report)) ->
            Docs.run $ Docs.Flags output report
          Right PackageValidate ->
            Validate.run
          Right (PackageBump (BumpFlags interactive)) ->
            Bump.run $ Bump.Flags (not interactive)
          Right PackageDiffLatest ->
            Diff.run Diff.CodeVsLatest
          Right (PackageDiffVersion version) ->
            Diff.run $ Diff.CodeVsExactly version
          Right (PackageDiffRange from to) ->
            Diff.run $ Diff.LocalInquiry from to
          Right (PackageDiffGlobal pkg from to) ->
            Diff.run $ Diff.GlobalInquiry pkg from to
      _ ->
        do
          putStrLn "Expected exactly 0 arguments."
          putStrLn ""
          putStrLn
            "It looks like you are trying to run Gren's internal backend directly.\
            \ To properly install Gren, see https://gren-lang.org/install"

data Command
  = Repl (Maybe String)
  | Make MakeFlags
  | Docs DocsFlags
  | PackageValidate
  | PackageBump BumpFlags
  | PackageDiffLatest
  | PackageDiffVersion Version.Version
  | PackageDiffRange Version.Version Version.Version
  | PackageDiffGlobal Package.Name Version.Version Version.Version
  deriving (Show)

data MakeFlags = MakeFlags
  { _make_optimize :: Bool,
    _make_sourcemaps :: Bool,
    _make_output :: Maybe Make.Output,
    _make_report_json :: Bool,
    _make_paths :: [String],
    _make_project_path :: String,
    _make_outline :: Outline,
    _make_root_sources :: Map ModuleName.Raw ByteString,
    _make_dependencies :: Map Package.Name Details.Dependency
  }
  deriving (Show)

data DocsFlags = DocsFlags
  { _docs_output :: Maybe Docs.Output,
    _docs_report_json :: Bool
  }
  deriving (Show)

data BumpFlags = BumpFlags
  { _bump_interactive :: Bool
  }
  deriving (Show)

data CommandDecoderError
  = UnknownCommand String
  | InvalidInput
  deriving (Show)

commandDecoder :: Json.Decoder CommandDecoderError Command
commandDecoder =
  do
    tipe <- Json.field (BS.pack "command") Json.string
    let commandStr = Utf8.toChars tipe
    case commandStr of
      "repl" -> Repl <$> maybeDecoder (fmap Utf8.toChars Json.string)
      "make" -> Make <$> makeDecoder
      "docs" -> Docs <$> docsDecoder
      "packageValidate" -> Json.succeed PackageValidate
      "packageBump" -> PackageBump <$> bumpDecoder
      "packageDiffLatest" -> Json.succeed PackageDiffLatest
      "packageDiffVersion" -> diffVersionDecoder
      "packageDiffRange" -> diffRangeDecoder
      "packageDiffGlobal" -> diffGlobalDecoder
      _ -> Json.failure (UnknownCommand $ Utf8.toChars tipe)

packageDecoder :: Json.Decoder CommandDecoderError Package.Name
packageDecoder =
  Json.mapError (const InvalidInput) Package.decoder

versionDecoder :: Json.Decoder CommandDecoderError Version.Version
versionDecoder =
  Json.mapError (const InvalidInput) Version.decoder

makeDecoder :: Json.Decoder CommandDecoderError MakeFlags
makeDecoder =
  MakeFlags
    <$> Json.field (BS.pack "optimize") Json.bool
    <*> Json.field (BS.pack "sourcemaps") Json.bool
    <*> Json.field (BS.pack "output") (maybeDecoder makeOutputDecoder)
    <*> Json.field (BS.pack "report-json") Json.bool
    <*> Json.field (BS.pack "entry-points") (Json.list (fmap Utf8.toChars Json.string))
    <*> Json.field (BS.pack "project-path") (fmap Utf8.toChars Json.string)
    <*> Json.field (BS.pack "project-outline") (Json.mapError (const InvalidInput) Outline.decoder)
    <*> Json.field (BS.pack "sources") (Json.dict (ModuleName.keyDecoder (\_ _ -> InvalidInput)) (fmap Utf8.toByteString Json.stringUnescaped))
    <*> Json.field (BS.pack "dependencies") (Json.dict (Package.keyDecoder (\_ _ -> InvalidInput)) makeDependencyDecoder)

makeOutputDecoder :: Json.Decoder CommandDecoderError Make.Output
makeOutputDecoder =
  do
    tipe <- Json.field (BS.pack "type") Json.string
    let commandStr = Utf8.toChars tipe
    case commandStr of
      "stdout" -> Json.succeed Make.DevStdOut
      "null" -> Json.succeed Make.DevNull
      "html" -> Make.Html <$> Json.field (BS.pack "path") (fmap Utf8.toChars Json.string)
      "js" -> Make.JS <$> Json.field (BS.pack "path") (fmap Utf8.toChars Json.string)
      "exe" -> Make.Exe <$> Json.field (BS.pack "path") (fmap Utf8.toChars Json.string)
      _ -> Json.failure InvalidInput

makeDependencyDecoder :: Json.Decoder CommandDecoderError Details.Dependency
makeDependencyDecoder =
  Details.Dependency
    <$> Json.field (BS.pack "outline") (Json.mapError (const InvalidInput) Outline.decoder)
    <*> Json.field (BS.pack "sources") (Json.dict (ModuleName.keyDecoder (\_ _ -> InvalidInput)) (fmap Utf8.toByteString Json.stringUnescaped))

docsDecoder :: Json.Decoder CommandDecoderError DocsFlags
docsDecoder =
  DocsFlags
    <$> Json.field (BS.pack "output") (maybeDecoder docsOutputDecoder)
    <*> Json.field (BS.pack "report-json") Json.bool

docsOutputDecoder :: Json.Decoder CommandDecoderError Docs.Output
docsOutputDecoder =
  do
    tipe <- Json.field (BS.pack "type") Json.string
    let commandStr = Utf8.toChars tipe
    case commandStr of
      "stdout" -> Json.succeed Docs.DevStdOut
      "null" -> Json.succeed Docs.DevNull
      "json" -> Docs.JSON <$> Json.field (BS.pack "path") (fmap Utf8.toChars Json.string)
      _ -> Json.failure InvalidInput

bumpDecoder :: Json.Decoder CommandDecoderError BumpFlags
bumpDecoder =
  BumpFlags
    <$> Json.field (BS.pack "interactive") Json.bool

diffVersionDecoder :: Json.Decoder CommandDecoderError Command
diffVersionDecoder =
  PackageDiffVersion
    <$> Json.field (BS.pack "version") versionDecoder

diffRangeDecoder :: Json.Decoder CommandDecoderError Command
diffRangeDecoder =
  PackageDiffRange
    <$> Json.field (BS.pack "from") versionDecoder
    <*> Json.field (BS.pack "to") versionDecoder

diffGlobalDecoder :: Json.Decoder CommandDecoderError Command
diffGlobalDecoder =
  PackageDiffGlobal
    <$> Json.field (BS.pack "package") packageDecoder
    <*> Json.field (BS.pack "from") versionDecoder
    <*> Json.field (BS.pack "to") versionDecoder

maybeDecoder :: Json.Decoder x a -> Json.Decoder x (Maybe a)
maybeDecoder subDecoder =
  Json.oneOf
    [ fmap Just subDecoder,
      Json.succeed Nothing
    ]
