{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Gren.Outline
  ( Outline (..),
    AppOutline (..),
    PkgOutline (..),
    Exposed (..),
    SrcDir (..),
    PossibleFilePath (..),
    read,
    write,
    encode,
    decoder,
    defaultSummary,
    flattenExposed,
    toAbsoluteSrcDir,
    toGiven,
    sourceDirs,
    platform,
    dependencyConstraints,
  )
where

import AbsoluteSrcDir (AbsoluteSrcDir)
import AbsoluteSrcDir qualified
import Control.Monad (filterM, liftM)
import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.NonEmptyList qualified as NE
import Data.OneOrMore qualified as OneOrMore
import File qualified
import Foreign.Ptr (minusPtr)
import Gren.Constraint qualified as Con
import Gren.Licenses qualified as Licenses
import Gren.ModuleName qualified as ModuleName
import Gren.Package qualified as Pkg
import Gren.Platform qualified as Platform
import Gren.PossibleFilePath (PossibleFilePath)
import Gren.PossibleFilePath qualified as PossibleFilePath
import Gren.Version qualified as V
import Json.Decode qualified as D
import Json.Encode ((==>))
import Json.Encode qualified as E
import Json.String qualified as Json
import Parse.Primitives qualified as P
import Reporting.Annotation qualified as A
import Reporting.Exit qualified as Exit
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as FP
import Prelude hiding (read)

-- OUTLINE

data Outline
  = App AppOutline
  | Pkg PkgOutline

data AppOutline = AppOutline
  { _app_gren_version :: V.Version,
    _app_platform :: Platform.Platform,
    _app_source_dirs :: NE.List SrcDir,
    _app_deps_direct :: Map.Map Pkg.Name (PossibleFilePath V.Version),
    _app_deps_indirect :: Map.Map Pkg.Name (PossibleFilePath V.Version)
  }

data PkgOutline = PkgOutline
  { _pkg_name :: Pkg.Name,
    _pkg_summary :: Json.String,
    _pkg_license :: Licenses.License,
    _pkg_version :: V.Version,
    _pkg_exposed :: Exposed,
    _pkg_deps :: Map.Map Pkg.Name (PossibleFilePath Con.Constraint),
    _pkg_gren_version :: Con.Constraint,
    _pkg_platform :: Platform.Platform
  }

data Exposed
  = ExposedList [ModuleName.Raw]
  | ExposedDict [(Json.String, [ModuleName.Raw])]

data SrcDir
  = AbsoluteSrcDir FilePath
  | RelativeSrcDir FilePath

-- DEFAULTS

defaultSummary :: Json.String
defaultSummary =
  Json.fromChars "helpful summary of your project, less than 80 characters"

-- HELPERS

flattenExposed :: Exposed -> [ModuleName.Raw]
flattenExposed exposed =
  case exposed of
    ExposedList names ->
      names
    ExposedDict sections ->
      concatMap snd sections

platform :: Outline -> Platform.Platform
platform outline =
  case outline of
    App (AppOutline _ pltform _ _ _) ->
      pltform
    Pkg (PkgOutline _ _ _ _ _ _ _ pltform) ->
      pltform

dependencyConstraints :: Outline -> Map.Map Pkg.Name (PossibleFilePath Con.Constraint)
dependencyConstraints outline =
  case outline of
    App appOutline ->
      let direct = _app_deps_direct appOutline
          indirect = _app_deps_indirect appOutline
          appDeps = Map.union direct indirect
       in Map.map (PossibleFilePath.mapWith Con.exactly) appDeps
    Pkg pkgOutline ->
      _pkg_deps pkgOutline

-- WRITE

write :: FilePath -> Outline -> IO ()
write root outline =
  E.write (root </> "gren.json") (encode outline)

-- JSON ENCODE

encode :: Outline -> E.Value
encode outline =
  case outline of
    App (AppOutline gren pltform srcDirs depsDirect depsTrans) ->
      E.object
        [ "type" ==> E.chars "application",
          "platform" ==> Platform.encode pltform,
          "source-directories" ==> E.list encodeSrcDir (NE.toList srcDirs),
          "gren-version" ==> V.encode gren,
          "dependencies"
            ==> E.object
              [ "direct" ==> encodeDeps V.encode depsDirect,
                "indirect" ==> encodeDeps V.encode depsTrans
              ]
        ]
    Pkg (PkgOutline name summary license version exposed deps gren pltform) ->
      E.object
        [ "type" ==> E.string (Json.fromChars "package"),
          "platform" ==> Platform.encode pltform,
          "name" ==> Pkg.encode name,
          "summary" ==> E.string summary,
          "license" ==> Licenses.encode license,
          "version" ==> V.encode version,
          "exposed-modules" ==> encodeExposed exposed,
          "gren-version" ==> Con.encode gren,
          "dependencies" ==> encodeDeps Con.encode deps
        ]

encodeExposed :: Exposed -> E.Value
encodeExposed exposed =
  case exposed of
    ExposedList modules ->
      E.list encodeModule modules
    ExposedDict chunks ->
      E.object (map (fmap (E.list encodeModule)) chunks)

encodeModule :: ModuleName.Raw -> E.Value
encodeModule name =
  E.name name

encodeDeps :: (a -> E.Value) -> Map.Map Pkg.Name (PossibleFilePath a) -> E.Value
encodeDeps encodeValue deps =
  E.dict Pkg.toJsonString (PossibleFilePath.encodeJson encodeValue) deps

encodeSrcDir :: SrcDir -> E.Value
encodeSrcDir srcDir =
  case srcDir of
    AbsoluteSrcDir dir -> E.chars dir
    RelativeSrcDir dir -> E.chars dir

-- PARSE AND VERIFY

read :: FilePath -> IO (Either Exit.Outline Outline)
read root =
  do
    bytes <- File.readUtf8 (root </> "gren.json")
    case D.fromByteString decoder bytes of
      Left err ->
        return $ Left (Exit.OutlineHasBadStructure err)
      Right outline ->
        case outline of
          Pkg (PkgOutline pkg _ _ _ _ deps _ _) ->
            return $
              if Map.notMember Pkg.core deps && pkg /= Pkg.core
                then Left Exit.OutlineNoPkgCore
                else Right outline
          App (AppOutline _ _ srcDirs direct _)
            | Map.notMember Pkg.core direct ->
                return $ Left Exit.OutlineNoAppCore
            | otherwise ->
                do
                  badDirs <- filterM (isSrcDirMissing root) (NE.toList srcDirs)
                  case map toGiven badDirs of
                    d : ds ->
                      return $ Left (Exit.OutlineHasMissingSrcDirs d ds)
                    [] ->
                      do
                        maybeDups <- detectDuplicates root (NE.toList srcDirs)
                        case maybeDups of
                          Nothing ->
                            return $ Right outline
                          Just (canonicalDir, (dir1, dir2)) ->
                            return $ Left (Exit.OutlineHasDuplicateSrcDirs canonicalDir dir1 dir2)

isSrcDirMissing :: FilePath -> SrcDir -> IO Bool
isSrcDirMissing root srcDir =
  not <$> Dir.doesDirectoryExist (toAbsolute root srcDir)

toGiven :: SrcDir -> FilePath
toGiven srcDir =
  case srcDir of
    AbsoluteSrcDir dir -> dir
    RelativeSrcDir dir -> dir

toAbsolute :: FilePath -> SrcDir -> FilePath
toAbsolute root srcDir =
  case srcDir of
    AbsoluteSrcDir dir -> dir
    RelativeSrcDir dir -> root </> dir

toAbsoluteSrcDir :: FilePath -> SrcDir -> IO AbsoluteSrcDir
toAbsoluteSrcDir root srcDir =
  AbsoluteSrcDir.fromFilePath (toAbsolute root srcDir)

detectDuplicates :: FilePath -> [SrcDir] -> IO (Maybe (FilePath, (FilePath, FilePath)))
detectDuplicates root srcDirs =
  do
    pairs <- traverse (toPair root) srcDirs
    return $
      Map.lookupMin $
        Map.mapMaybe isDup $
          Map.fromListWith OneOrMore.more pairs

toPair :: FilePath -> SrcDir -> IO (FilePath, OneOrMore.OneOrMore FilePath)
toPair root srcDir =
  do
    key <- Dir.canonicalizePath (toAbsolute root srcDir)
    return (key, OneOrMore.one (toGiven srcDir))

isDup :: OneOrMore.OneOrMore FilePath -> Maybe (FilePath, FilePath)
isDup paths =
  case paths of
    OneOrMore.One _ -> Nothing
    OneOrMore.More a b -> Just (OneOrMore.getFirstTwo a b)

sourceDirs :: Outline -> NE.List SrcDir
sourceDirs outline =
  case outline of
    App (AppOutline _ _ srcDirs _ _) ->
      srcDirs
    Pkg _ ->
      NE.singleton (RelativeSrcDir "src")

-- JSON DECODE

type Decoder a =
  D.Decoder Exit.OutlineProblem a

decoder :: Decoder Outline
decoder =
  let application = Json.fromChars "application"
      package = Json.fromChars "package"
   in do
        tipe <- D.field "type" D.string
        if
            | tipe == application -> App <$> appDecoder
            | tipe == package -> Pkg <$> pkgDecoder
            | otherwise -> D.failure Exit.OP_BadType

appDecoder :: Decoder AppOutline
appDecoder =
  AppOutline
    <$> D.field "gren-version" versionDecoder
    <*> D.field "platform" (Platform.decoder Exit.OP_BadPlatform)
    <*> D.field "source-directories" dirsDecoder
    <*> D.field "dependencies" (D.field "direct" (depsDecoder versionOrFilePathDecoder))
    <*> D.field "dependencies" (D.field "indirect" (depsDecoder versionOrFilePathDecoder))

pkgDecoder :: Decoder PkgOutline
pkgDecoder =
  PkgOutline
    <$> D.field "name" nameDecoder
    <*> D.field "summary" summaryDecoder
    <*> D.field "license" (Licenses.decoder Exit.OP_BadLicense)
    <*> D.field "version" versionDecoder
    <*> D.field "exposed-modules" exposedDecoder
    <*> D.field "dependencies" (depsDecoder constraintOrFilePathDecoder)
    <*> D.field "gren-version" constraintDecoder
    <*> D.field "platform" (Platform.decoder Exit.OP_BadPlatform)

-- JSON DECODE HELPERS

nameDecoder :: Decoder Pkg.Name
nameDecoder =
  D.mapError (uncurry Exit.OP_BadPkgName) Pkg.decoder

summaryDecoder :: Decoder Json.String
summaryDecoder =
  D.customString
    (boundParser 80 Exit.OP_BadSummaryTooLong)
    (\_ _ -> Exit.OP_BadSummaryTooLong)

versionDecoder :: Decoder V.Version
versionDecoder =
  D.mapError (Exit.OP_BadVersion . Exit.OP_AttemptedOther) V.decoder

versionOrFilePathDecoder :: Decoder (PossibleFilePath V.Version)
versionOrFilePathDecoder =
  D.oneOf
    [ do
        vsn <- D.mapError (Exit.OP_BadVersion . Exit.OP_AttemptedOther) V.decoder
        D.succeed (PossibleFilePath.Other vsn),
      filePathDecoder Exit.OP_BadVersion
    ]

filePathDecoder :: (Exit.PossibleFilePath err -> Exit.OutlineProblem) -> Decoder (PossibleFilePath val)
filePathDecoder errorMapper =
  do
    jsonStr <- D.string
    D.Decoder $ \(A.At errRegion@(A.Region (A.Position row col) _) _) ok err ->
      let filePath = Json.toChars jsonStr
       in if List.isPrefixOf localDepPrefix filePath
            then ok (PossibleFilePath.Is $ List.drop (List.length localDepPrefix) filePath)
            else err (D.Failure errRegion $ errorMapper $ Exit.OP_AttemptedFilePath (row, col))

localDepPrefix :: String
localDepPrefix =
  "local:"

constraintDecoder :: Decoder Con.Constraint
constraintDecoder =
  D.mapError (Exit.OP_BadConstraint . Exit.OP_AttemptedOther) Con.decoder

constraintOrFilePathDecoder :: Decoder (PossibleFilePath Con.Constraint)
constraintOrFilePathDecoder =
  D.oneOf
    [ do
        con <- D.mapError (Exit.OP_BadConstraint . Exit.OP_AttemptedOther) Con.decoder
        D.succeed (PossibleFilePath.Other con),
      filePathDecoder Exit.OP_BadConstraint
    ]

depsDecoder :: Decoder a -> Decoder (Map.Map Pkg.Name a)
depsDecoder valueDecoder =
  D.dict (Pkg.keyDecoder Exit.OP_BadDependencyName) valueDecoder

dirsDecoder :: Decoder (NE.List SrcDir)
dirsDecoder =
  fmap (toSrcDir . Json.toChars) <$> D.nonEmptyList D.string Exit.OP_NoSrcDirs

toSrcDir :: FilePath -> SrcDir
toSrcDir path =
  if FP.isRelative path
    then RelativeSrcDir path
    else AbsoluteSrcDir path

-- EXPOSED MODULES DECODER

exposedDecoder :: Decoder Exposed
exposedDecoder =
  D.oneOf
    [ ExposedList <$> D.list moduleDecoder,
      ExposedDict <$> D.pairs headerKeyDecoder (D.list moduleDecoder)
    ]

moduleDecoder :: Decoder ModuleName.Raw
moduleDecoder =
  D.mapError (uncurry Exit.OP_BadModuleName) ModuleName.decoder

headerKeyDecoder :: D.KeyDecoder Exit.OutlineProblem Json.String
headerKeyDecoder =
  D.KeyDecoder
    (boundParser 20 Exit.OP_BadModuleHeaderTooLong)
    (\_ _ -> Exit.OP_BadModuleHeaderTooLong)

-- BOUND PARSER

boundParser :: Int -> x -> P.Parser x Json.String
boundParser bound tooLong =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr _ ->
    let len = minusPtr end pos
        newCol = col + fromIntegral len
     in if len < bound
          then cok (Json.fromPtr pos end) (P.State src end end indent row newCol)
          else cerr row newCol (\_ _ -> tooLong)

-- BINARY

instance Binary SrcDir where
  put outline =
    case outline of
      AbsoluteSrcDir a -> putWord8 0 >> put a
      RelativeSrcDir a -> putWord8 1 >> put a

  get =
    do
      n <- getWord8
      case n of
        0 -> liftM AbsoluteSrcDir get
        1 -> liftM RelativeSrcDir get
        _ -> fail "binary encoding of SrcDir was corrupted"
