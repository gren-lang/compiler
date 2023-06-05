{-# LANGUAGE OverloadedStrings #-}

module Make
  ( Flags (..),
    Output (..),
    ReportType (..),
    run,
    reportType,
    output,
    rereadSources,
  )
where

import AST.Optimized qualified as Opt
import BackgroundWriter qualified as BW
import Build qualified
import Data.ByteString.Builder qualified as B
import Data.Map (Map)
import Data.Maybe qualified as Maybe
import Data.NonEmptyList qualified as NE
import Directories qualified as Dirs
import File qualified
import Generate qualified
import Generate.Html qualified as Html
import Generate.JavaScript qualified as JS
import Generate.Node qualified as Node
import Generate.SourceMap qualified as SourceMap
import Gren.Details qualified as Details
import Gren.ModuleName qualified as ModuleName
import Gren.Outline qualified as Outline
import Gren.Platform qualified as Platform
import Parse.Module qualified as Parse
import Reporting qualified
import Reporting.Exit qualified as Exit
import Reporting.Task qualified as Task
import System.Directory qualified as Dir
import System.FilePath qualified as FP
import System.IO qualified as IO
import Terminal (Parser (..))

-- FLAGS

data Flags = Flags
  { _debug :: Bool,
    _optimize :: Bool,
    _output :: Maybe Output,
    _report :: Maybe ReportType
  }

data Output
  = Exe FilePath
  | JS FilePath
  | Html FilePath
  | DevNull
  | DevStdOut

data ReportType
  = Json

-- RUN

type Task a = Task.Task Exit.Make a

run :: [FilePath] -> Flags -> IO ()
run paths flags@(Flags _ _ maybeOutput report) =
  do
    style <- getStyle maybeOutput report
    maybeRoot <- Dirs.findRoot
    Reporting.attemptWithStyle style Exit.makeToReport $
      case maybeRoot of
        Just root -> runHelp root paths style flags
        Nothing -> return $ Left Exit.MakeNoOutline

runHelp :: FilePath -> [FilePath] -> Reporting.Style -> Flags -> IO (Either Exit.Make ())
runHelp root paths style (Flags debug optimize maybeOutput _) =
  BW.withScope $ \scope ->
    Dirs.withRootLock root $
      Task.run $
        do
          desiredMode <- getMode debug optimize
          details <- Task.eio Exit.MakeBadDetails (Details.load style scope root)
          moduleSources <- Task.io $ rereadSources root
          let platform = getPlatform details
          let projectType = getProjectType details
          case (projectType, maybeOutput) of
            (Parse.Package _, Just _) ->
              Task.throw Exit.MakeCannotOutputForPackage
            _ ->
              case paths of
                [] ->
                  do
                    exposed <- getExposed details
                    buildExposed style root details exposed
                p : ps ->
                  do
                    artifacts <- buildPaths style root details (NE.List p ps)
                    let mains = getMains artifacts
                    case (projectType, mains) of
                      (Parse.Package _, m : ms) ->
                        Task.throw $ Exit.MakeCannotOutputMainForPackage m ms
                      _ ->
                        case maybeOutput of
                          Nothing ->
                            case (platform, mains) of
                              (_, []) ->
                                return ()
                              (Platform.Browser, [name]) ->
                                do
                                  (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                                  writeToDisk style "index.html" (Html.sandwich name (SourceMap.generateOnto Html.leadingLines moduleSources sourceMap source)) (NE.List name [])
                              (Platform.Node, [name]) ->
                                do
                                  (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                                  writeToDisk style "app" (SourceMap.generateOnto Node.leadingLines moduleSources sourceMap (Node.sandwich name source)) (NE.List name [])
                              (_, name : names) ->
                                do
                                  (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                                  writeToDisk style "index.js" (SourceMap.generateOnto 0 moduleSources sourceMap source) (NE.List name names)
                          Just DevStdOut ->
                            case getMains artifacts of
                              [] ->
                                return ()
                              _ ->
                                do
                                  (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                                  Task.io $ B.hPutBuilder IO.stdout (SourceMap.generateOnto 0 moduleSources sourceMap source)
                          Just DevNull ->
                            return ()
                          Just (Exe target) ->
                            case platform of
                              Platform.Node -> do
                                name <- hasOneMain artifacts
                                (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                                writeToDisk style target (SourceMap.generateOnto Node.leadingLines moduleSources sourceMap (Node.sandwich name source)) (NE.List name [])
                              _ -> do
                                Task.throw Exit.MakeExeOnlyForNodePlatform
                          Just (JS target) ->
                            case getNoMains artifacts of
                              [] -> do
                                (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                                writeToDisk style target (SourceMap.generateOnto 0 moduleSources sourceMap source) (Build.getRootNames artifacts)
                              name : names ->
                                Task.throw (Exit.MakeNonMainFilesIntoJavaScript name names)
                          Just (Html target) ->
                            case platform of
                              Platform.Browser -> do
                                name <- hasOneMain artifacts
                                (JS.GeneratedResult source sourceMap) <- generate root details desiredMode artifacts
                                writeToDisk style target (Html.sandwich name (SourceMap.generateOnto Html.leadingLines moduleSources sourceMap source)) (NE.List name [])
                              _ -> do
                                Task.throw Exit.MakeHtmlOnlyForBrowserPlatform

-- GET INFORMATION

getStyle :: Maybe Output -> Maybe ReportType -> IO Reporting.Style
getStyle maybeOutput report =
  case (maybeOutput, report) of
    (Just DevStdOut, _) -> return Reporting.silent
    (_, Nothing) -> Reporting.terminal
    (_, Just Json) -> return Reporting.json

getMode :: Bool -> Bool -> Task DesiredMode
getMode debug optimize =
  case (debug, optimize) of
    (True, True) -> Task.throw Exit.MakeCannotOptimizeAndDebug
    (True, False) -> return Debug
    (False, False) -> return Dev
    (False, True) -> return Prod

rereadSources :: FilePath -> IO (Map ModuleName.Canonical String)
rereadSources root =
  do
    modulePaths <- Outline.getAllModulePaths root
    traverse readFile modulePaths

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

getProjectType :: Details.Details -> Parse.ProjectType
getProjectType (Details.Details _ validOutline _ _ _ _) = do
  case validOutline of
    Details.ValidApp _ _ ->
      Parse.Application
    Details.ValidPkg _ name _ ->
      Parse.Package name

-- BUILD PROJECTS

buildExposed :: Reporting.Style -> FilePath -> Details.Details -> NE.List ModuleName.Raw -> Task ()
buildExposed style root details exposed =
  Task.eio Exit.MakeCannotBuild $
    Build.fromExposed style root details Build.IgnoreDocs exposed

buildPaths :: Reporting.Style -> FilePath -> Details.Details -> NE.List FilePath -> Task Build.Artifacts
buildPaths style root details paths =
  Task.eio Exit.MakeCannotBuild $
    Build.fromPaths style root details paths

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

writeToDisk :: Reporting.Style -> FilePath -> B.Builder -> NE.List ModuleName.Raw -> Task ()
writeToDisk style target builder names =
  Task.io $
    do
      Dir.createDirectoryIfMissing True (FP.takeDirectory target)
      File.writeBuilder target builder
      Reporting.reportGenerate style names target

-- GENERATE

data DesiredMode = Debug | Dev | Prod

generate :: FilePath -> Details.Details -> DesiredMode -> Build.Artifacts -> Task JS.GeneratedResult
generate root details desiredMode artifacts =
  Task.mapError Exit.MakeBadGenerate $
    case desiredMode of
      Debug -> Generate.debug root details artifacts
      Dev -> Generate.dev root details artifacts
      Prod -> Generate.prod root details artifacts

-- PARSERS

reportType :: Parser ReportType
reportType =
  Parser
    { _singular = "report type",
      _plural = "report types",
      _parser = \string -> if string == "json" then Just Json else Nothing,
      _suggest = \_ -> return ["json"],
      _examples = \_ -> return ["json"]
    }

output :: Parser Output
output =
  Parser
    { _singular = "output file",
      _plural = "output files",
      _parser = parseOutput,
      _suggest = \_ -> return [],
      _examples = \_ -> return ["gren.js", "index.html", "/dev/null", "/dev/stdout"]
    }

parseOutput :: String -> Maybe Output
parseOutput name
  | name == "/dev/stdout" = Just DevStdOut
  | isDevNull name = Just DevNull
  | hasExt ".html" name = Just (Html name)
  | hasExt ".js" name = Just (JS name)
  | noExt name = Just (Exe name)
  | otherwise = Nothing

hasExt :: String -> String -> Bool
hasExt ext path =
  FP.takeExtension path == ext && length path > length ext

noExt :: String -> Bool
noExt path =
  FP.takeExtension path == ""

isDevNull :: String -> Bool
isDevNull name =
  name == "/dev/null" || name == "NUL" || name == "$null"
