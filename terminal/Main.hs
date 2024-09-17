module Main
  ( main,
  )
where

import Data.ByteString.Char8 qualified as BS
import Data.Utf8 qualified as Utf8
import Docs qualified
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Gren.Package qualified as Package
import Gren.Platform qualified as Platform
import Gren.Version qualified as Version
import Init qualified
import Json.Decode qualified as Json
import Make qualified
import Package.Bump qualified as Bump
import Package.Install qualified as Install
import Package.Outdated qualified as Outdated
import Package.Uninstall qualified as Uninstall
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
      [json] ->
        let jsonByteString = BS.pack json
         in case Json.fromByteString commandDecoder jsonByteString of
              Left err ->
                error (show err)
              Right (Init (InitFlags interactive package platform)) ->
                Init.run $ Init.Flags (not interactive) package platform
              Right (Repl interpreter) ->
                Repl.run $ Repl.Flags interpreter
              Right (Make (MakeFlags debug optimize sourcemaps output report paths)) ->
                Make.run paths $ Make.Flags debug optimize sourcemaps output report
              Right (Docs (DocsFlags output report)) ->
                -- TODO: fix errors
                Docs.run $ Docs.Flags output report
              Right (PackageInstall (InstallFlags interactive Nothing)) ->
                Install.run Install.NoArgs $ Install.Flags (not interactive)
              Right (PackageInstall (InstallFlags interactive (Just packageName))) ->
                Install.run (Install.Install packageName) $ Install.Flags (not interactive)
              Right (PackageUninstall (UninstallFlags interactive packageName)) ->
                Uninstall.run packageName $ Uninstall.Flags (not interactive)
              Right PackageOutdated ->
                Outdated.run
              Right PackageValidate ->
                Validate.run
              Right (PackageBump (BumpFlags interactive)) ->
                Bump.run $ Bump.Flags (not interactive)
              Right command ->
                print command
      _ ->
        putStrLn "Expected exactly 1 argument: a json-encoded command"

data Command
  = Init InitFlags
  | Repl (Maybe String)
  | Make MakeFlags
  | Docs DocsFlags
  | PackageInstall InstallFlags
  | PackageUninstall UninstallFlags
  | PackageOutdated
  | PackageValidate
  | PackageBump BumpFlags
  | PackageDiffLatest
  | PackageDiffVersion Version.Version
  | PackageDiffRange Version.Version Version.Version
  | PackageDiffGlobal Package.Name Version.Version Version.Version
  deriving (Show)

data InitFlags = InitFlags
  { _init_interactive :: Bool,
    _init_package :: Bool,
    _init_platform :: Platform.Platform
  }
  deriving (Show)

data MakeFlags = MakeFlags
  { _make_debug :: Bool,
    _make_optimize :: Bool,
    _make_sourcemaps :: Bool,
    _make_output :: Maybe Make.Output,
    _make_report_json :: Bool,
    _make_paths :: [String]
  }
  deriving (Show)

data DocsFlags = DocsFlags
  { _docs_output :: Maybe Docs.Output,
    _docs_report_json :: Bool
  }
  deriving (Show)

data InstallFlags = InstallFlags
  { _install_interactive :: Bool,
    _install_package :: Maybe Package.Name
  }
  deriving (Show)

data UninstallFlags = UninstallFlags
  { _uninstall_interactive :: Bool,
    _uninstall_package :: Package.Name
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
      "init" -> Init <$> initDecoder
      "repl" -> Repl <$> maybeDecoder (fmap Utf8.toChars Json.string)
      "make" -> Make <$> makeDecoder
      "docs" -> Docs <$> docsDecoder
      "packageInstall" -> PackageInstall <$> installDecoder
      "packageUninstall" -> PackageUninstall <$> uninstallDecoder
      "packageOutdated" -> Json.succeed PackageOutdated
      "packageValidate" -> Json.succeed PackageValidate
      "packageBump" -> PackageBump <$> bumpDecoder
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
    <$> Json.field (BS.pack "interactive") Json.bool
    <*> Json.field (BS.pack "package") Json.bool
    <*> Json.field (BS.pack "platform") (Platform.decoder InvalidInput)

makeDecoder :: Json.Decoder CommandDecoderError MakeFlags
makeDecoder =
  MakeFlags
    <$> Json.field (BS.pack "debug") Json.bool
    <*> Json.field (BS.pack "optimize") Json.bool
    <*> Json.field (BS.pack "sourcemaps") Json.bool
    <*> Json.field (BS.pack "output") (maybeDecoder makeOutputDecoder)
    <*> Json.field (BS.pack "report-json") Json.bool
    <*> Json.field (BS.pack "paths") (Json.list (fmap Utf8.toChars Json.string))

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

installDecoder :: Json.Decoder CommandDecoderError InstallFlags
installDecoder =
  InstallFlags
    <$> Json.field (BS.pack "interactive") Json.bool
    <*> Json.field (BS.pack "package") (maybeDecoder packageDecoder)

uninstallDecoder :: Json.Decoder CommandDecoderError UninstallFlags
uninstallDecoder =
  UninstallFlags
    <$> Json.field (BS.pack "interactive") Json.bool
    <*> Json.field (BS.pack "package") packageDecoder

bumpDecoder :: Json.Decoder CommandDecoderError BumpFlags
bumpDecoder =
  BumpFlags
    <$> Json.field (BS.pack "interactive") Json.bool

maybeDecoder :: Json.Decoder x a -> Json.Decoder x (Maybe a)
maybeDecoder subDecoder =
  Json.oneOf
    [ fmap Just subDecoder,
      Json.succeed Nothing
    ]
