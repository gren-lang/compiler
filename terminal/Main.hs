{-# LANGUAGE MultiWayIf #-}

module Main
  ( main,
  )
where

import Data.ByteString.Char8 qualified as BS
import Data.Utf8 qualified as Utf8
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Gren.Package qualified as Package
import Gren.Platform qualified as Platform
import Gren.Version qualified as Version
import Json.Decode qualified as Json
import Json.String qualified as JS
import System.Environment qualified as Env

-- MAIN

main :: IO ()
main =
  do
    setLocaleEncoding utf8
    argStrings <- Env.getArgs
    case argStrings of
      [json] ->
        let jsonByteString = BS.pack json
         in case Json.fromByteString commandDecoder jsonByteString of
              Left err ->
                error (show err)
              Right command ->
                print command
      _ ->
        putStrLn "Expected exactly 1 argument: a json-encoded command"

data Command
  = Init InitFlags
  | Repl (Maybe String)
  | Make MakeFlags
  | Docs DocsFlags
  | PackageInstall (Maybe Package.Name)
  | PackageUninstall Package.Name
  | PackageOutdated
  | PackageValidate
  | PackageBump
  | PackageDiffLatest
  | PackageDiffVersion Version.Version
  | PackageDiffRange Version.Version Version.Version
  | PackageDiffGlobal Package.Name Version.Version Version.Version
  deriving (Show)

data InitFlags = InitFlags
  { _init_package :: Bool,
    _init_platform :: Platform.Platform
  }
  deriving (Show)

data MakeFlags = MakeFlags
  { _make_debug :: Bool,
    _make_optimize :: Bool,
    _make_sourcemaps :: Bool,
    _make_output :: Maybe JS.String,
    _make_report_json :: Bool
  }
  deriving (Show)

data DocsFlags = DocsFlags
  { _docs_output :: Maybe JS.String,
    _docs_report_json :: Bool
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
      "init" -> Init <$> initDecoder
      "repl" -> Repl <$> maybeDecoder (fmap Utf8.toChars Json.string)
      "make" -> Make <$> makeDecoder
      "docs" -> Docs <$> docsDecoder
      "packageInstall" -> PackageInstall <$> maybeDecoder packageDecoder
      "packageUninstall" -> PackageUninstall <$> packageDecoder
      "packageOutdated" -> Json.succeed PackageOutdated
      "packageValidate" -> Json.succeed PackageValidate
      "packageBump" -> Json.succeed PackageBump
      "packageDiffLatest" -> Json.succeed PackageDiffLatest
      "packageDiffVersion" -> PackageDiffVersion <$> versionDecoder
      "packageDiffRange" -> PackageDiffRange <$> versionDecoder <*> versionDecoder
      "packageDiffRange" -> PackageDiffGlobal <$> packageDecoder <*> versionDecoder <*> versionDecoder
      _ -> Json.failure (UnknownCommand $ Utf8.toChars tipe)

packageDecoder :: Json.Decoder CommandDecoderError Package.Name
packageDecoder =
  Json.mapError (const InvalidInput) Package.decoder

versionDecoder :: Json.Decoder CommandDecoderError Version.Version
versionDecoder =
  Json.mapError (const InvalidInput) Version.decoder

initDecoder :: Json.Decoder CommandDecoderError InitFlags
initDecoder =
  InitFlags
    <$> Json.field (BS.pack "package") Json.bool
    <*> Json.field (BS.pack "platform") (Platform.decoder InvalidInput)

makeDecoder :: Json.Decoder CommandDecoderError MakeFlags
makeDecoder =
  MakeFlags
    <$> Json.field (BS.pack "debug") Json.bool
    <*> Json.field (BS.pack "optimize") Json.bool
    <*> Json.field (BS.pack "sourcemaps") Json.bool
    <*> Json.field (BS.pack "output") (maybeDecoder Json.string)
    <*> Json.field (BS.pack "report-json") Json.bool

docsDecoder :: Json.Decoder CommandDecoderError DocsFlags
docsDecoder =
  DocsFlags
    <$> Json.field (BS.pack "output") (maybeDecoder Json.string)
    <*> Json.field (BS.pack "report-json") Json.bool

maybeDecoder :: Json.Decoder x a -> Json.Decoder x (Maybe a)
maybeDecoder subDecoder =
  Json.oneOf
    [ fmap Just subDecoder,
      Json.succeed Nothing
    ]
