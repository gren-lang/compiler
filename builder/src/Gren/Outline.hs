{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Gren.Outline
  ( Outline (..),
    AppOutline (..),
    PkgOutline (..),
    Exposed (..),
    SrcDir (..),
    read,
    write,
    encode,
    decoder,
    defaultSummary,
    flattenExposed,
    toAbsoluteSrcDir,
    sourceDirs,
    testDirs,
  )
where

import AbsoluteSrcDir (AbsoluteSrcDir)
import qualified AbsoluteSrcDir
import Control.Monad (filterM, liftM)
import Data.Binary (Binary, get, getWord8, put, putWord8)
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import qualified File
import Foreign.Ptr (minusPtr)
import qualified Gren.Constraint as Con
import qualified Gren.Licenses as Licenses
import qualified Gren.ModuleName as ModuleName
import qualified Gren.Package as Pkg
import qualified Gren.Version as V
import qualified Json.Decode as D
import Json.Encode ((==>))
import qualified Json.Encode as E
import qualified Json.String as Json
import qualified Parse.Primitives as P
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as FP
import Prelude hiding (read)

-- OUTLINE

data Outline
  = App AppOutline
  | Pkg PkgOutline

data AppOutline = AppOutline
  { _app_gren_version :: V.Version,
    _app_source_dirs :: NE.List SrcDir,
    _app_deps_direct :: Map.Map Pkg.Name V.Version,
    _app_deps_indirect :: Map.Map Pkg.Name V.Version,
    _app_test_direct :: Map.Map Pkg.Name V.Version,
    _app_test_indirect :: Map.Map Pkg.Name V.Version
  }

data PkgOutline = PkgOutline
  { _pkg_name :: Pkg.Name,
    _pkg_summary :: Json.String,
    _pkg_license :: Licenses.License,
    _pkg_version :: V.Version,
    _pkg_exposed :: Exposed,
    _pkg_deps :: Map.Map Pkg.Name Con.Constraint,
    _pkg_test_deps :: Map.Map Pkg.Name Con.Constraint,
    _pkg_gren_version :: Con.Constraint
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

-- WRITE

write :: FilePath -> Outline -> IO ()
write root outline =
  E.write (root </> "gren.json") (encode outline)

-- JSON ENCODE

encode :: Outline -> E.Value
encode outline =
  case outline of
    App (AppOutline gren srcDirs depsDirect depsTrans testDirect testTrans) ->
      E.object
        [ "type" ==> E.chars "application",
          "source-directories" ==> E.list encodeSrcDir (NE.toList srcDirs),
          "gren-version" ==> V.encode gren,
          "dependencies"
            ==> E.object
              [ "direct" ==> encodeDeps V.encode depsDirect,
                "indirect" ==> encodeDeps V.encode depsTrans
              ],
          "test-dependencies"
            ==> E.object
              [ "direct" ==> encodeDeps V.encode testDirect,
                "indirect" ==> encodeDeps V.encode testTrans
              ]
        ]
    Pkg (PkgOutline name summary license version exposed deps tests gren) ->
      E.object
        [ "type" ==> E.string (Json.fromChars "package"),
          "name" ==> Pkg.encode name,
          "summary" ==> E.string summary,
          "license" ==> Licenses.encode license,
          "version" ==> V.encode version,
          "exposed-modules" ==> encodeExposed exposed,
          "gren-version" ==> Con.encode gren,
          "dependencies" ==> encodeDeps Con.encode deps,
          "test-dependencies" ==> encodeDeps Con.encode tests
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

encodeDeps :: (a -> E.Value) -> Map.Map Pkg.Name a -> E.Value
encodeDeps encodeValue deps =
  E.dict Pkg.toJsonString encodeValue deps

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
          App (AppOutline _ srcDirs direct indirect _ _)
            | Map.notMember Pkg.core direct ->
                return $ Left Exit.OutlineNoAppCore
            | Map.notMember Pkg.json direct && Map.notMember Pkg.json indirect ->
                return $ Left Exit.OutlineNoAppJson
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
    App (AppOutline _ srcDirs _ _ _ _) ->
      srcDirs
    Pkg _ ->
      NE.singleton (RelativeSrcDir "src")

testDirs :: Outline -> NE.List SrcDir
testDirs _ =
  NE.singleton (RelativeSrcDir "tests")

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
    <*> D.field "source-directories" dirsDecoder
    <*> D.field "dependencies" (D.field "direct" (depsDecoder versionDecoder))
    <*> D.field "dependencies" (D.field "indirect" (depsDecoder versionDecoder))
    <*> D.field "test-dependencies" (D.field "direct" (depsDecoder versionDecoder))
    <*> D.field "test-dependencies" (D.field "indirect" (depsDecoder versionDecoder))

pkgDecoder :: Decoder PkgOutline
pkgDecoder =
  PkgOutline
    <$> D.field "name" nameDecoder
    <*> D.field "summary" summaryDecoder
    <*> D.field "license" (Licenses.decoder Exit.OP_BadLicense)
    <*> D.field "version" versionDecoder
    <*> D.field "exposed-modules" exposedDecoder
    <*> D.field "dependencies" (depsDecoder constraintDecoder)
    <*> D.field "test-dependencies" (depsDecoder constraintDecoder)
    <*> D.field "gren-version" constraintDecoder

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
  D.mapError (uncurry Exit.OP_BadVersion) V.decoder

constraintDecoder :: Decoder Con.Constraint
constraintDecoder =
  D.mapError Exit.OP_BadConstraint Con.decoder

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
