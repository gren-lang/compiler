module Command
  ( Command (..),
    commandDecoder,
    ReplFlags (..),
    MakeFlags (..),
    DocsFlags (..),
    BumpFlags (..),
    ValidateFlags (..),
    DiffFlags (..),
    ProjectInfo (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map (Map)
import Data.Utf8 qualified as Utf8
import Docs qualified
import Gren.Details qualified as Details
import Gren.ModuleName qualified as ModuleName
import Gren.Outline (Outline)
import Gren.Outline qualified as Outline
import Gren.Package qualified as Package
import Gren.Version qualified as Version
import Json.Decode qualified as Json
import Make qualified as Make

data Command
  = Repl ReplFlags
  | Make MakeFlags
  | Docs DocsFlags
  | PackageValidate ValidateFlags
  | PackageBump BumpFlags
  | PackageDiff DiffFlags
  deriving (Show)

data ReplFlags = ReplFlags
  { _repl_interpreter :: Maybe String,
    _repl_project_path :: FilePath,
    _repl_outline :: Outline,
    _repl_root_sources :: Map ModuleName.Raw ByteString,
    _repl_dependencies :: Map Package.Name Details.Dependency
  }
  deriving (Show)

data MakeFlags = MakeFlags
  { _make_optimize :: Bool,
    _make_sourcemaps :: Bool,
    _make_output :: Maybe Make.Output,
    _make_report_json :: Bool,
    _make_paths :: [ModuleName.Raw],
    _make_project_path :: String,
    _make_outline :: Outline,
    _make_root_sources :: Map ModuleName.Raw ByteString,
    _make_dependencies :: Map Package.Name Details.Dependency
  }
  deriving (Show)

data DocsFlags = DocsFlags
  { _docs_output :: Maybe Docs.Output,
    _docs_report_json :: Bool,
    _docs_project_path :: String,
    _docs_outline :: Outline,
    _docs_root_sources :: Map ModuleName.Raw ByteString,
    _docs_dependencies :: Map Package.Name Details.Dependency
  }
  deriving (Show)

data BumpFlags = BumpFlags
  { _bump_interactive :: Bool,
    _bump_project_path :: String,
    _bump_known_versions :: [Version.Version],
    _bump_current :: ProjectInfo,
    _bump_published :: ProjectInfo
  }
  deriving (Show)

data ValidateFlags = ValidateFlags
  { _validate_project_path :: String,
    _validate_known_versions :: [Version.Version],
    _validate_current :: ProjectInfo,
    _validate_previous :: Maybe ProjectInfo
  }
  deriving (Show)

data ProjectInfo = ProjectInfo
  { _project_outline :: Outline,
    _project_root_sources :: Map ModuleName.Raw ByteString,
    _project_dependencies :: Map Package.Name Details.Dependency
  }
  deriving (Show)

data DiffFlags = DiffFlags
  { _diff_interactive :: Bool,
    _diff_project_path :: String,
    _diff_first :: ProjectInfo,
    _diff_second :: ProjectInfo
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
      "repl" -> Repl <$> replDecoder
      "make" -> Make <$> makeDecoder
      "docs" -> Docs <$> docsDecoder
      "packageValidate" -> PackageValidate <$> validateDecoder
      "packageBump" -> PackageBump <$> bumpDecoder
      "packageDiff" -> PackageDiff <$> diffDecoder
      _ -> Json.failure (UnknownCommand $ Utf8.toChars tipe)

versionDecoder :: Json.Decoder CommandDecoderError Version.Version
versionDecoder =
  Json.mapError (const InvalidInput) Version.decoder

replDecoder :: Json.Decoder CommandDecoderError ReplFlags
replDecoder =
  ReplFlags
    <$> Json.field (BS.pack "interpreter") (maybeDecoder (fmap Utf8.toChars Json.string))
    <*> Json.field (BS.pack "project-path") (fmap Utf8.toChars Json.string)
    <*> Json.field (BS.pack "project-outline") (Json.mapError (const InvalidInput) Outline.decoder)
    <*> Json.field (BS.pack "sources") (Json.dict (ModuleName.keyDecoder (\_ _ -> InvalidInput)) (fmap Utf8.toByteString Json.stringUnescaped))
    <*> Json.field (BS.pack "dependencies") (Json.dict (Package.keyDecoder (\_ _ -> InvalidInput)) makeDependencyDecoder)

makeDecoder :: Json.Decoder CommandDecoderError MakeFlags
makeDecoder =
  MakeFlags
    <$> Json.field (BS.pack "optimize") Json.bool
    <*> Json.field (BS.pack "sourcemaps") Json.bool
    <*> Json.field (BS.pack "output") (maybeDecoder makeOutputDecoder)
    <*> Json.field (BS.pack "report-json") Json.bool
    <*> Json.field (BS.pack "entry-points") (Json.list (Json.mapError (const InvalidInput) ModuleName.decoder))
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
    <*> Json.field (BS.pack "project-path") (fmap Utf8.toChars Json.string)
    <*> Json.field (BS.pack "project-outline") (Json.mapError (const InvalidInput) Outline.decoder)
    <*> Json.field (BS.pack "sources") (Json.dict (ModuleName.keyDecoder (\_ _ -> InvalidInput)) (fmap Utf8.toByteString Json.stringUnescaped))
    <*> Json.field (BS.pack "dependencies") (Json.dict (Package.keyDecoder (\_ _ -> InvalidInput)) makeDependencyDecoder)

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
    <*> Json.field (BS.pack "project-path") (fmap Utf8.toChars Json.string)
    <*> Json.field (BS.pack "known-versions") (Json.list versionDecoder)
    <*> Json.field (BS.pack "current-version") projectInfoDecoder
    <*> Json.field (BS.pack "published-version") projectInfoDecoder

validateDecoder :: Json.Decoder CommandDecoderError ValidateFlags
validateDecoder =
  ValidateFlags
    <$> Json.field (BS.pack "project-path") (fmap Utf8.toChars Json.string)
    <*> Json.field (BS.pack "known-versions") (Json.list versionDecoder)
    <*> Json.field (BS.pack "current-version") projectInfoDecoder
    <*> Json.field (BS.pack "previous-version") (maybeDecoder projectInfoDecoder)

projectInfoDecoder :: Json.Decoder CommandDecoderError ProjectInfo
projectInfoDecoder =
  ProjectInfo
    <$> Json.field (BS.pack "project-outline") (Json.mapError (const InvalidInput) Outline.decoder)
    <*> Json.field (BS.pack "sources") (Json.dict (ModuleName.keyDecoder (\_ _ -> InvalidInput)) (fmap Utf8.toByteString Json.stringUnescaped))
    <*> Json.field (BS.pack "dependencies") (Json.dict (Package.keyDecoder (\_ _ -> InvalidInput)) makeDependencyDecoder)

diffDecoder :: Json.Decoder CommandDecoderError DiffFlags
diffDecoder =
  DiffFlags
    <$> Json.field (BS.pack "interactive") Json.bool
    <*> Json.field (BS.pack "project-path") (fmap Utf8.toChars Json.string)
    <*> Json.field (BS.pack "first-package") projectInfoDecoder
    <*> Json.field (BS.pack "second-package") projectInfoDecoder

maybeDecoder :: Json.Decoder x a -> Json.Decoder x (Maybe a)
maybeDecoder subDecoder =
  Json.oneOf
    [ fmap Just subDecoder,
      Json.succeed Nothing
    ]
