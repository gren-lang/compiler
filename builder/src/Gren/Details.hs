{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Gren.Details
  ( Details (..),
    BuildID,
    ValidOutline (..),
    Dependency (..),
    Local (..),
    Foreign (..),
    load,
    loadObjects,
    loadInterfaces,
  )
where

import AST.Canonical qualified as Can
import AST.Optimized qualified as Opt
import AST.Source qualified as Src
import Compile qualified
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (liftM2, liftM3)
import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.ByteString.Internal (ByteString)
import Data.Either qualified as Either
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Utils qualified as Map
import Data.Maybe qualified as Maybe
import Data.Name qualified as Name
import Data.NonEmptyList qualified as NE
import Data.OneOrMore qualified as OneOrMore
import Data.Set qualified as Set
import Data.Word (Word64)
import Directories qualified as Dirs
import File qualified
import Gren.Docs qualified as Docs
import Gren.Interface qualified as I
import Gren.Kernel qualified as Kernel
import Gren.ModuleName qualified as ModuleName
import Gren.Outline (Outline)
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Gren.Platform qualified as P
import Gren.Version qualified as V
import Parse.Module qualified as Parse
import Reporting.Annotation qualified as A
import Reporting.Exit qualified as Exit
import Reporting.Task qualified as Task

-- DETAILS

data Details = Details
  { _outlineTime :: File.Time,
    _outline :: ValidOutline,
    _buildID :: BuildID,
    _locals :: Map.Map ModuleName.Raw Local,
    _foreigns :: Map.Map ModuleName.Raw Foreign,
    _extras :: Extras
  }

type BuildID = Word64

data ValidOutline
  = ValidApp P.Platform (NE.List Outline.SrcDir)
  | ValidPkg P.Platform Pkg.Name [ModuleName.Raw]

data Dependency = Dependency
  { _dep_outline :: Outline,
    _dep_sources :: Map ModuleName.Raw ByteString
  }
  deriving (Show)

-- NOTE: we need two ways to detect if a file must be recompiled:
--
-- (1) _time is the modification time from the last time we compiled the file.
-- By checking EQUALITY with the current modification time, we can detect file
-- saves and `git checkout` of previous versions. Both need a recompile.
--
-- (2) _lastChange is the BuildID from the last time a new interface file was
-- generated, and _lastCompile is the BuildID from the last time the file was
-- compiled. These may be different if a file is recompiled but the interface
-- stayed the same. When the _lastCompile is LESS THAN the _lastChange of any
-- imports, we need to recompile. This can happen when a project has multiple
-- entrypoints and some modules are compiled less often than their imports.
--
data Local = Local
  { _path :: FilePath,
    _deps :: [ModuleName.Raw],
    _main :: Bool
  }

data Foreign
  = Foreign Pkg.Name [Pkg.Name]

data Extras
  = ArtifactsCached
  | ArtifactsFresh Interfaces Opt.GlobalGraph

type Interfaces =
  Map.Map ModuleName.Canonical I.DependencyInterface

-- LOAD ARTIFACTS

loadObjects :: FilePath -> Details -> IO (MVar (Maybe Opt.GlobalGraph))
loadObjects root (Details _ _ _ _ _ extras) =
  case extras of
    ArtifactsFresh _ o -> newMVar (Just o)
    ArtifactsCached -> fork (File.readBinary (Dirs.objects root))

loadInterfaces :: FilePath -> Details -> IO (MVar (Maybe Interfaces))
loadInterfaces root (Details _ _ _ _ _ extras) =
  case extras of
    ArtifactsFresh i _ -> newMVar (Just i)
    ArtifactsCached -> fork (File.readBinary (Dirs.interfaces root))

-- LOAD -- used by Make, Docs, Repl

load :: Outline.Outline -> Map.Map Pkg.Name Dependency -> IO (Either Exit.Details Details)
load outline solution =
  case outline of
    Outline.Pkg (Outline.PkgOutline pkg _ _ _ exposed direct _ rootPlatform) ->
      Task.run $
        do
          let exposedList = Outline.flattenExposed exposed
          verifyDependencies (ValidPkg rootPlatform pkg exposedList) solution direct
    Outline.App (Outline.AppOutline _ rootPlatform srcDirs direct _) ->
      Task.run $
        verifyDependencies (ValidApp rootPlatform srcDirs) solution direct

type Task a = Task.Task Exit.Details a

-- FORK

fork :: IO a -> IO (MVar a)
fork work =
  do
    mvar <- newEmptyMVar
    _ <- forkIO $ putMVar mvar =<< work
    return mvar

-- VERIFY DEPENDENCIES

verifyDependencies :: ValidOutline -> Map.Map Pkg.Name Dependency -> Map.Map Pkg.Name a -> Task Details
verifyDependencies outline solution directDeps =
  Task.eio id $
    do
      mvar <- newEmptyMVar
      mvars <- Map.traverseWithKey (\k v -> fork (build mvar k v)) solution
      putMVar mvar mvars
      deps <- traverse readMVar mvars
      case sequence deps of
        Left _ ->
          do
            home <- Dirs.getGrenHome
            return $
              Left $
                Exit.DetailsBadDeps home $
                  Maybe.catMaybes $
                    Either.lefts $
                      Map.elems deps
        Right artifacts ->
          let objs = Map.foldr addObjects Opt.empty artifacts
              ifaces = Map.foldrWithKey (addInterfaces directDeps) Map.empty artifacts
              foreigns = Map.map (OneOrMore.destruct Foreign) $ Map.foldrWithKey gatherForeigns Map.empty $ Map.intersection artifacts directDeps
              details = Details File.zeroTime outline 0 Map.empty foreigns (ArtifactsFresh ifaces objs)
           in return $ Right details

addObjects :: Artifacts -> Opt.GlobalGraph -> Opt.GlobalGraph
addObjects (Artifacts _ objs) graph =
  Opt.addGlobalGraph objs graph

addInterfaces :: Map.Map Pkg.Name a -> Pkg.Name -> Artifacts -> Interfaces -> Interfaces
addInterfaces directDeps pkg (Artifacts ifaces _) dependencyInterfaces =
  Map.union dependencyInterfaces $
    Map.mapKeysMonotonic (ModuleName.Canonical pkg) $
      if Map.member pkg directDeps
        then ifaces
        else Map.map I.privatize ifaces

gatherForeigns :: Pkg.Name -> Artifacts -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name) -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name)
gatherForeigns pkg (Artifacts ifaces _) foreigns =
  let isPublic di =
        case di of
          I.Public _ -> Just (OneOrMore.one pkg)
          I.Private _ _ _ -> Nothing
   in Map.unionWith OneOrMore.more foreigns (Map.mapMaybe isPublic ifaces)

-- VERIFY DEPENDENCY

data Artifacts = Artifacts
  { _ifaces :: Map.Map ModuleName.Raw I.DependencyInterface,
    _objects :: Opt.GlobalGraph
  }

type Dep =
  Either (Maybe Exit.DetailsBadDep) Artifacts

-- ARTIFACT CACHE

data ArtifactCache = ArtifactCache
  { _fingerprints :: Set.Set Fingerprint,
    _artifacts :: Artifacts
  }

type Fingerprint =
  Map.Map Pkg.Name V.Version

-- BUILD

build :: MVar (Map.Map Pkg.Name (MVar Dep)) -> Pkg.Name -> Dependency -> IO Dep
build depsMVar pkg (Dependency outline sources) =
  case outline of
    (Outline.App _) ->
      do
        return $ Left $ Just $ Exit.BD_BadBuild pkg V.one Map.empty
    (Outline.Pkg (Outline.PkgOutline _ _ _ _ exposed deps _ platform)) ->
      do
        allDeps <- readMVar depsMVar
        directDeps <- traverse readMVar (Map.intersection allDeps deps)
        case sequence directDeps of
          Left _ ->
            do
              return $ Left Nothing
          Right directArtifacts ->
            do
              let foreignDeps = gatherForeignInterfaces directArtifacts
              let exposedDict = Map.fromKeys (const ()) (Outline.flattenExposed exposed)
              let docsStatus = DocsNeeded
              let authorizedForKernelCode = Pkg.isKernel pkg
              mvar <- newEmptyMVar
              mvars <- Map.traverseWithKey (const . fork . crawlModule foreignDeps sources mvar pkg docsStatus authorizedForKernelCode) exposedDict
              putMVar mvar mvars
              mapM_ readMVar mvars
              maybeStatuses <- traverse readMVar =<< readMVar mvar
              case sequence maybeStatuses of
                Left CrawlCorruption ->
                  do
                    return $ Left $ Just $ Exit.BD_BadBuild pkg V.one Map.empty
                Left CrawlUnsignedKernelCode ->
                  do
                    return $ Left $ Just $ Exit.BD_UnsignedBuild pkg V.one
                Right statuses ->
                  do
                    rmvar <- newEmptyMVar
                    rmvars <- traverse (fork . compile platform pkg rmvar) statuses
                    putMVar rmvar rmvars
                    maybeResults <- traverse readMVar rmvars
                    case sequence maybeResults of
                      Nothing ->
                        do
                          return $ Left $ Just $ Exit.BD_BadBuild pkg V.one Map.empty
                      Just results ->
                        let ifaces = gatherInterfaces exposedDict results
                            objects = gatherObjects results
                            artifacts = Artifacts ifaces objects
                         in do
                              return (Right artifacts)

-- GATHER

gatherObjects :: Map.Map ModuleName.Raw Result -> Opt.GlobalGraph
gatherObjects results =
  Map.foldrWithKey addLocalGraph Opt.empty results

addLocalGraph :: ModuleName.Raw -> Result -> Opt.GlobalGraph -> Opt.GlobalGraph
addLocalGraph name status graph =
  case status of
    RLocal _ objs _ -> Opt.addLocalGraph objs graph
    RForeign _ -> graph
    RKernelLocal cs -> Opt.addKernel (Name.getKernel name) cs graph
    RKernelForeign -> graph

gatherInterfaces :: Map.Map ModuleName.Raw () -> Map.Map ModuleName.Raw Result -> Map.Map ModuleName.Raw I.DependencyInterface
gatherInterfaces exposed artifacts =
  let onLeft = Map.mapMissing (error "compiler bug manifesting in Gren.Details.gatherInterfaces")
      onRight = Map.mapMaybeMissing (\_ iface -> toLocalInterface I.private iface)
      onBoth = Map.zipWithMaybeMatched (\_ () iface -> toLocalInterface I.public iface)
   in Map.merge onLeft onRight onBoth exposed artifacts

toLocalInterface :: (I.Interface -> a) -> Result -> Maybe a
toLocalInterface func result =
  case result of
    RLocal iface _ _ -> Just (func iface)
    RForeign _ -> Nothing
    RKernelLocal _ -> Nothing
    RKernelForeign -> Nothing

-- GATHER FOREIGN INTERFACES

data ForeignInterface
  = ForeignAmbiguous
  | ForeignSpecific I.Interface

gatherForeignInterfaces :: Map.Map Pkg.Name Artifacts -> Map.Map ModuleName.Raw ForeignInterface
gatherForeignInterfaces directArtifacts =
  Map.map (OneOrMore.destruct finalize) $
    Map.foldrWithKey gather Map.empty directArtifacts
  where
    finalize :: I.Interface -> [I.Interface] -> ForeignInterface
    finalize i is =
      case is of
        [] -> ForeignSpecific i
        _ : _ -> ForeignAmbiguous

    gather :: Pkg.Name -> Artifacts -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore I.Interface) -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore I.Interface)
    gather _ (Artifacts ifaces _) buckets =
      Map.unionWith OneOrMore.more buckets (Map.mapMaybe isPublic ifaces)

    isPublic :: I.DependencyInterface -> Maybe (OneOrMore.OneOrMore I.Interface)
    isPublic di =
      case di of
        I.Public iface -> Just (OneOrMore.one iface)
        I.Private _ _ _ -> Nothing

-- CRAWL

type StatusDict =
  Map.Map ModuleName.Raw (MVar (Either CrawlError Status))

data Status
  = SLocal DocsStatus (Map.Map ModuleName.Raw ()) Src.Module
  | SForeign I.Interface
  | SKernelLocal [Kernel.Chunk]
  | SKernelForeign

data CrawlError
  = CrawlUnsignedKernelCode
  | CrawlCorruption

crawlModule :: Map.Map ModuleName.Raw ForeignInterface -> Map.Map ModuleName.Raw ByteString -> MVar StatusDict -> Pkg.Name -> DocsStatus -> Bool -> ModuleName.Raw -> IO (Either CrawlError Status)
crawlModule foreignDeps sources mvar pkg docsStatus authorizedForKernelCode name =
  case (Map.lookup name foreignDeps, Map.lookup name sources) of
    (Just ForeignAmbiguous, _) ->
      return $ Left CrawlCorruption
    (Just (ForeignSpecific iface), Nothing) ->
      return $ Right (SForeign iface)
    (Just (ForeignSpecific _), Just _) ->
      return $ Left CrawlCorruption
    (_, Just bytes) ->
      if Pkg.isKernel pkg && Name.isKernel name
        then
          if authorizedForKernelCode
            then crawlKernel foreignDeps sources mvar pkg bytes
            else return $ Left CrawlUnsignedKernelCode
        else crawlFile foreignDeps sources mvar pkg docsStatus authorizedForKernelCode name bytes
    (Nothing, Nothing) ->
      if Pkg.isKernel pkg && Name.isKernel name && authorizedForKernelCode
        then return $ Right SKernelForeign
        else return $ Left CrawlCorruption

crawlFile :: Map.Map ModuleName.Raw ForeignInterface -> Map.Map ModuleName.Raw ByteString -> MVar StatusDict -> Pkg.Name -> DocsStatus -> Bool -> ModuleName.Raw -> ByteString -> IO (Either CrawlError Status)
crawlFile foreignDeps sources mvar pkg docsStatus authorizedForKernelCode expectedName bytes =
  case Parse.fromByteString (Parse.Package pkg) bytes of
    Right modul@(Src.Module (Just (A.At _ actualName)) _ _ imports _ _ _ _ _ _ _) | expectedName == actualName ->
      do
        deps <- crawlImports foreignDeps sources mvar pkg authorizedForKernelCode (fmap snd imports)
        return (Right (SLocal docsStatus deps modul))
    _ ->
      return $ Left CrawlCorruption

crawlImports :: Map.Map ModuleName.Raw ForeignInterface -> Map.Map ModuleName.Raw ByteString -> MVar StatusDict -> Pkg.Name -> Bool -> [Src.Import] -> IO (Map.Map ModuleName.Raw ())
crawlImports foreignDeps sources mvar pkg authorizedForKernelCode imports =
  do
    statusDict <- takeMVar mvar
    let deps = Map.fromList (map (\i -> (Src.getImportName i, ())) imports)
    let news = Map.difference deps statusDict
    mvars <- Map.traverseWithKey (const . fork . crawlModule foreignDeps sources mvar pkg DocsNotNeeded authorizedForKernelCode) news
    putMVar mvar (Map.union mvars statusDict)
    mapM_ readMVar mvars
    return deps

crawlKernel :: Map.Map ModuleName.Raw ForeignInterface -> Map.Map ModuleName.Raw ByteString -> MVar StatusDict -> Pkg.Name -> ByteString -> IO (Either CrawlError Status)
crawlKernel foreignDeps sources mvar pkg bytes =
  case Kernel.fromByteString pkg (Map.mapMaybe getDepHome foreignDeps) bytes of
    Nothing ->
      return $ Left CrawlCorruption
    Just (Kernel.Content imports chunks) ->
      do
        _ <- crawlImports foreignDeps sources mvar pkg True imports
        return (Right (SKernelLocal chunks))

getDepHome :: ForeignInterface -> Maybe Pkg.Name
getDepHome fi =
  case fi of
    ForeignSpecific (I.Interface pkg _ _ _ _) -> Just pkg
    ForeignAmbiguous -> Nothing

-- COMPILE

data Result
  = RLocal !I.Interface !Opt.LocalGraph (Maybe Docs.Module)
  | RForeign I.Interface
  | RKernelLocal [Kernel.Chunk]
  | RKernelForeign

compile :: P.Platform -> Pkg.Name -> MVar (Map.Map ModuleName.Raw (MVar (Maybe Result))) -> Status -> IO (Maybe Result)
compile platform pkg mvar status =
  case status of
    SLocal docsStatus deps modul ->
      do
        resultsDict <- readMVar mvar
        maybeResults <- traverse readMVar (Map.intersection resultsDict deps)
        case sequence maybeResults of
          Nothing ->
            return Nothing
          Just results ->
            case Compile.compile platform pkg (Map.mapMaybe getInterface results) modul of
              Left _ ->
                return Nothing
              Right (Compile.Artifacts canonical annotations objects) ->
                let ifaces = I.fromModule pkg canonical annotations
                    docs = makeDocs docsStatus canonical
                 in return (Just (RLocal ifaces objects docs))
    SForeign iface ->
      return (Just (RForeign iface))
    SKernelLocal chunks ->
      return (Just (RKernelLocal chunks))
    SKernelForeign ->
      return (Just RKernelForeign)

getInterface :: Result -> Maybe I.Interface
getInterface result =
  case result of
    RLocal iface _ _ -> Just iface
    RForeign iface -> Just iface
    RKernelLocal _ -> Nothing
    RKernelForeign -> Nothing

-- MAKE DOCS

data DocsStatus
  = DocsNeeded
  | DocsNotNeeded

makeDocs :: DocsStatus -> Can.Module -> Maybe Docs.Module
makeDocs status modul =
  case status of
    DocsNeeded ->
      case Docs.fromModule modul of
        Right docs -> Just docs
        Left _ -> Nothing
    DocsNotNeeded ->
      Nothing

-- BINARY

instance Binary Details where
  put (Details a b c d e _) = put a >> put b >> put c >> put d >> put e
  get =
    do
      a <- get
      b <- get
      c <- get
      d <- get
      e <- get
      return (Details a b c d e ArtifactsCached)

instance Binary ValidOutline where
  put outline =
    case outline of
      ValidApp a b -> putWord8 0 >> put a >> put b
      ValidPkg a b c -> putWord8 1 >> put a >> put b >> put c

  get =
    do
      n <- getWord8
      case n of
        0 -> liftM2 ValidApp get get
        1 -> liftM3 ValidPkg get get get
        _ -> fail "binary encoding of ValidOutline was corrupted"

instance Binary Local where
  put (Local a b c) = put a >> put b >> put c
  get =
    do
      a <- get
      b <- get
      c <- get
      return (Local a b c)

instance Binary Foreign where
  get = liftM2 Foreign get get
  put (Foreign a b) = put a >> put b

instance Binary Artifacts where
  get = liftM2 Artifacts get get
  put (Artifacts a b) = put a >> put b

instance Binary ArtifactCache where
  get = liftM2 ArtifactCache get get
  put (ArtifactCache a b) = put a >> put b
