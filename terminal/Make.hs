module Make
  ( Flags (..),
    Output (..),
    run,
  )
where

import AST.Optimized qualified as Opt
import Build qualified
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as ByteString8
import Data.ByteString.Internal (ByteString)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.NonEmptyList qualified as NE
import File qualified
import Generate qualified
import Generate.Html qualified as Html
import Generate.JavaScript qualified as JS
import Generate.Node qualified as Node
import Generate.SourceMap (SourceMap)
import Generate.SourceMap qualified as SourceMap
import Gren.Details qualified as Details
import Gren.ModuleName qualified as ModuleName
import Gren.Outline (Outline)
import Gren.Outline qualified as Outline
import Gren.Package qualified as Package
import Gren.Platform qualified as Platform
import Reporting qualified
import Reporting.Exit qualified as Exit
import Reporting.Task qualified as Task
import System.Directory qualified as Dir
import System.FilePath qualified as FP
import System.IO qualified as IO

-- FLAGS

data Flags = Flags
  { _optimize :: Bool,
    _sourceMaps :: Bool,
    _output :: Maybe Output,
    _report :: Bool,
    _paths :: [ModuleName.Raw],
    _project_path :: String,
    _outline :: Outline,
    _root_sources :: Map ModuleName.Raw ByteString,
    _dependencies :: Map Package.Name Details.Dependency
  }
  deriving (Show)

data Output
  = Exe FilePath
  | JS FilePath
  | Html FilePath
  | DevNull
  | DevStdOut
  deriving (Show)

-- RUN

type Task a = Task.Task Exit.Make a

run :: Flags -> IO ()
run flags@(Flags _ _ maybeOutput report _ _ _ _ _) =
  do
    style <- getStyle maybeOutput report
    -- TODO: File locking in frontend
    -- TODO: Show error for Exit.MakeNoOutline in frontend
    Reporting.attemptWithStyle style Exit.makeToReport $
      runHelp style flags

runHelp :: Reporting.Style -> Flags -> IO (Either Exit.Make ())
runHelp style flags@(Flags optimize withSourceMaps maybeOutput _ modules root outline sources deps) =
  Task.run $
    do
      desiredMode <- getMode optimize
      details <- Task.eio Exit.MakeBadDetails (Details.loadForMake style outline deps)
      let platform = getPlatform details
      case modules of
        [] ->
          do
            exposed <- getExposed details
            buildExposed style root details sources exposed
        p : ps ->
          do
            artifacts <- buildPaths style root details sources (NE.List p ps)
            let mains = getMains artifacts
            case maybeOutput of
              Nothing ->
                case (platform, mains) of
                  (_, []) ->
                    return ()
                  (Platform.Browser, [name]) ->
                    do
                      (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                      let bundle = prepareOutput withSourceMaps flags Html.leadingLines sourceMap source
                      writeToDisk style "index.html" (Html.sandwich name bundle) (NE.List name [])
                  (Platform.Node, [name]) ->
                    do
                      (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                      let bundle = prepareOutput withSourceMaps flags Node.leadingLines sourceMap (Node.sandwich name source)
                      writeToDisk style "app" bundle (NE.List name [])
                  (_, name : names) ->
                    do
                      (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                      let bundle = prepareOutput withSourceMaps flags 0 sourceMap source
                      writeToDisk style "index.js" bundle (NE.List name names)
              Just DevStdOut ->
                case getMains artifacts of
                  [] ->
                    return ()
                  _ ->
                    do
                      (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                      let bundle = prepareOutput withSourceMaps flags 0 sourceMap source
                      Task.io $ B.hPutBuilder IO.stdout bundle
              Just DevNull ->
                return ()
              Just (Exe target) ->
                case platform of
                  Platform.Node -> do
                    name <- hasOneMain artifacts
                    (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                    let bundle = prepareOutput withSourceMaps flags Node.leadingLines sourceMap (Node.sandwich name source)
                    writeToDisk style target bundle (NE.List name [])
                  _ -> do
                    Task.throw Exit.MakeExeOnlyForNodePlatform
              Just (JS target) ->
                case getNoMains artifacts of
                  [] -> do
                    (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                    let bundle = prepareOutput withSourceMaps flags 0 sourceMap source
                    writeToDisk style target bundle (Build.getRootNames artifacts)
                  name : names ->
                    Task.throw (Exit.MakeNonMainFilesIntoJavaScript name names)
              Just (Html target) ->
                case platform of
                  Platform.Browser -> do
                    name <- hasOneMain artifacts
                    (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                    let bundle = prepareOutput withSourceMaps flags Html.leadingLines sourceMap source
                    writeToDisk style target (Html.sandwich name bundle) (NE.List name [])
                  _ -> do
                    Task.throw Exit.MakeHtmlOnlyForBrowserPlatform

-- GET INFORMATION

getStyle :: Maybe Output -> Bool -> IO Reporting.Style
getStyle maybeOutput report =
  case (maybeOutput, report) of
    (Just DevStdOut, _) -> return Reporting.silent
    (_, False) -> Reporting.terminal
    (_, True) -> return Reporting.json

getMode :: Bool -> Task DesiredMode
getMode optimize =
  return (if optimize then Prod else Dev)

getExposed :: Details.Details -> Task (NE.List ModuleName.Raw)
getExposed (Details.Details _ validOutline _ _ _ _) =
  case validOutline of
    Details.ValidApp _ _ ->
      Task.throw Exit.MakeAppNeedsFileNames
    Details.ValidPkg _ _ exposed ->
      case exposed of
        [] -> Task.throw Exit.MakePkgNeedsExposing
        m : ms -> return (NE.List m ms)

getPlatform :: Details.Details -> Platform.Platform
getPlatform (Details.Details _ validOutline _ _ _ _) = do
  case validOutline of
    Details.ValidApp platform _ ->
      platform
    Details.ValidPkg platform _ _ ->
      platform

-- BUILD PROJECTS

buildExposed :: Reporting.Style -> FilePath -> Details.Details -> Map ModuleName.Raw ByteString -> NE.List ModuleName.Raw -> Task ()
buildExposed style root details sources exposed =
  Task.eio Exit.MakeCannotBuild $
    Build.fromExposedSources style root details sources Build.IgnoreDocs exposed

buildPaths :: Reporting.Style -> FilePath -> Details.Details -> Map ModuleName.Raw ByteString -> NE.List ModuleName.Raw -> Task Build.Artifacts
buildPaths style root details sources modules =
  Task.eio Exit.MakeCannotBuild $
    Build.fromMainModules style root details sources modules

-- GET MAINS

getMains :: Build.Artifacts -> [ModuleName.Raw]
getMains (Build.Artifacts _ _ roots modules) =
  Maybe.mapMaybe (getMain modules) (NE.toList roots)

getMain :: [Build.Module] -> Build.Root -> Maybe ModuleName.Raw
getMain modules root =
  case root of
    Build.Inside name ->
      if any (isMain name) modules
        then Just name
        else Nothing
    Build.Outside name _ (Opt.LocalGraph maybeMain _ _) ->
      case maybeMain of
        Just _ -> Just name
        Nothing -> Nothing

isMain :: ModuleName.Raw -> Build.Module -> Bool
isMain targetName modul =
  case modul of
    Build.Fresh name _ (Opt.LocalGraph maybeMain _ _) ->
      Maybe.isJust maybeMain && name == targetName
    Build.Cached name mainIsDefined _ ->
      mainIsDefined && name == targetName

-- HAS ONE MAIN

hasOneMain :: Build.Artifacts -> Task ModuleName.Raw
hasOneMain (Build.Artifacts _ _ roots modules) =
  case roots of
    NE.List root [] -> Task.mio Exit.MakeNoMain (return $ getMain modules root)
    NE.List _ (_ : _) -> Task.throw Exit.MakeMultipleFiles

-- GET MAINLESS

getNoMains :: Build.Artifacts -> [ModuleName.Raw]
getNoMains (Build.Artifacts _ _ roots modules) =
  Maybe.mapMaybe (getNoMain modules) (NE.toList roots)

getNoMain :: [Build.Module] -> Build.Root -> Maybe ModuleName.Raw
getNoMain modules root =
  case root of
    Build.Inside name ->
      if any (isMain name) modules
        then Nothing
        else Just name
    Build.Outside name _ (Opt.LocalGraph maybeMain _ _) ->
      case maybeMain of
        Just _ -> Nothing
        Nothing -> Just name

-- WRITE TO DISK

prepareOutput :: Bool -> Flags -> Int -> SourceMap -> B.Builder -> B.Builder
prepareOutput enabled flags leadingLines sourceMap source =
  if enabled
    then
      let moduleSources = gatherSources flags
       in SourceMap.generateOnto leadingLines moduleSources sourceMap source
    else source

gatherSources :: Flags -> Map ModuleName.Canonical String
gatherSources (Flags _ _ _ _ _ _ outline sources deps) =
  let mappedSources =
        case outline of
          Outline.App _ ->
            Map.mapKeys (ModuleName.Canonical Package.dummyName) sources
          Outline.Pkg pkgOutline ->
            Map.mapKeys (ModuleName.Canonical (Outline._pkg_name pkgOutline)) sources

      mappedModules =
        Map.foldrWithKey
          ( \packageName depSources acc ->
              Map.union
                (Map.mapKeys (ModuleName.Canonical packageName) (Details._dep_sources depSources))
                acc
          )
          mappedSources
          deps
   in Map.map ByteString8.unpack mappedModules

writeToDisk :: Reporting.Style -> FilePath -> B.Builder -> NE.List ModuleName.Raw -> Task ()
writeToDisk style target builder names =
  Task.io $
    do
      Dir.createDirectoryIfMissing True (FP.takeDirectory target)
      File.writeBuilder target builder
      Reporting.reportGenerate style names target

-- GENERATE

data DesiredMode = Dev | Prod

generate :: FilePath -> Details.Details -> DesiredMode -> Build.Artifacts -> Task JS.GeneratedResult
generate root details desiredMode artifacts =
  Task.mapError Exit.MakeBadGenerate $
    case desiredMode of
      Dev -> Generate.dev root details artifacts
      Prod -> Generate.prod root details artifacts
