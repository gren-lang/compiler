{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Gren.Details
  ( Details (..),
    BuildID,
    ValidOutline (..),
    Local (..),
    Foreign (..),
    load,
    loadObjects,
    loadInterfaces,
    verifyInstall,
  )
where

import AST.Canonical qualified as Can
import AST.Optimized qualified as Opt
import AST.Source qualified as Src
import BackgroundWriter qualified as BW
import Compile qualified
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (liftM2, liftM3)
import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.Either qualified as Either
import Data.Map qualified as Map
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Utils qualified as Map
import Data.Maybe qualified as Maybe
import Data.Name qualified as Name
import Data.NonEmptyList qualified as NE
import Data.OneOrMore qualified as OneOrMore
import Data.Set qualified as Set
import Data.Word (Word64)
import Deps.Solver qualified as Solver
import Directories qualified as Dirs
import File qualified
import Gren.Constraint qualified as Con
import Gren.Docs qualified as Docs
import Gren.Interface qualified as I
import Gren.Kernel qualified as Kernel
import Gren.ModuleName qualified as ModuleName
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Gren.Platform qualified as P
import Gren.Platform qualified as Platform
import Gren.PossibleFilePath (PossibleFilePath)
import Gren.PossibleFilePath qualified as PossibleFilePath
import Gren.Version qualified as V
import Json.Encode qualified as E
import Parse.Module qualified as Parse
import Reporting qualified
import Reporting.Annotation qualified as A
import Reporting.Exit qualified as Exit
import Reporting.Task qualified as Task
import System.FilePath ((<.>), (</>))

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
    _time :: File.Time,
    _deps :: [ModuleName.Raw],
    _main :: Bool,
    _lastChange :: BuildID,
    _lastCompile :: BuildID
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

-- VERIFY INSTALL -- used by Install

verifyInstall :: BW.Scope -> FilePath -> Solver.Env -> Outline.Outline -> IO (Either Exit.Details Details)
verifyInstall scope root (Solver.Env cache) outline =
  do
    time <- File.getTime (root </> "gren.json")
    let key = Reporting.ignorer
    let env = Env key scope root cache
    generate env outline time

-- LOAD -- used by Make, Docs, Repl

load :: Reporting.Style -> BW.Scope -> FilePath -> IO (Either Exit.Details Details)
load style scope root =
  Reporting.trackDetails style $ \key ->
    do
      result <- initEnv key scope root
      case result of
        Left exit ->
          return (Left exit)
        Right (env, outline) ->
          do
            newTime <- File.getTime (root </> "gren.json")
            maybeDetails <- File.readBinary (Dirs.details root)
            case maybeDetails of
              Nothing ->
                generate env outline newTime
              Just details@(Details oldTime _ buildID _ _ _) ->
                if oldTime == newTime && not (containsLocalDeps outline)
                  then return (Right details {_buildID = buildID + 1})
                  else generate env outline newTime

containsLocalDeps :: Outline.Outline -> Bool
containsLocalDeps outline =
  case outline of
    Outline.Pkg pkg ->
      any PossibleFilePath.is $ Map.elems (Outline._pkg_deps pkg)
    Outline.App app ->
      any PossibleFilePath.is $ Map.elems (Map.union (Outline._app_deps_direct app) (Outline._app_deps_indirect app))

-- GENERATE

generate :: Env -> Outline.Outline -> File.Time -> IO (Either Exit.Details Details)
generate env outline time =
  case outline of
    Outline.Pkg pkg -> Task.run (verifyPkg env time pkg)
    Outline.App app -> Task.run (verifyApp env time app)

-- ENV

data Env = Env
  { _key :: Reporting.DKey,
    _scope :: BW.Scope,
    _root :: FilePath,
    _cache :: Dirs.PackageCache
  }

initEnv :: Reporting.DKey -> BW.Scope -> FilePath -> IO (Either Exit.Details (Env, Outline.Outline))
initEnv key scope root =
  do
    mvar <- fork Solver.initEnv
    eitherOutline <- Outline.read root
    case eitherOutline of
      Left problem ->
        return $ Left $ Exit.DetailsBadOutline problem
      Right outline ->
        do
          (Solver.Env cache) <- readMVar mvar
          return $ Right (Env key scope root cache, outline)

-- VERIFY PROJECT

type Task a = Task.Task Exit.Details a

verifyPkg :: Env -> File.Time -> Outline.PkgOutline -> Task Details
verifyPkg env@(Env reportKey _ _ _) time (Outline.PkgOutline pkg _ _ _ exposed direct gren rootPlatform) =
  if Con.goodGren gren
    then do
      _ <- Task.io $ Reporting.report reportKey $ Reporting.DStart $ Map.size direct
      solution <-
        verifyConstraints
          env
          rootPlatform
          (Map.map (PossibleFilePath.mapWith (Con.exactly . Con.lowerBound)) direct)
      let exposedList = Outline.flattenExposed exposed
      verifyDependencies env time (ValidPkg rootPlatform pkg exposedList) solution direct
    else Task.throw $ Exit.DetailsBadGrenInPkg gren

verifyApp :: Env -> File.Time -> Outline.AppOutline -> Task Details
verifyApp env@(Env reportKey _ _ _) time (Outline.AppOutline grenVersion rootPlatform srcDirs direct indirect) =
  if grenVersion == V.compiler
    then do
      stated <- union noDups direct indirect
      _ <- Task.io $ Reporting.report reportKey $ Reporting.DStart (Map.size stated)
      actual <- verifyConstraints env rootPlatform (Map.map (PossibleFilePath.mapWith Con.exactly) stated)
      if Map.size stated == Map.size actual
        then verifyDependencies env time (ValidApp rootPlatform srcDirs) actual direct
        else
          let actualVersions = Map.map (\(Solver.Details vsn _ _) -> vsn) actual
           in Task.throw $
                Exit.DetailsMissingDeps $
                  Map.toList $
                    Map.difference actualVersions stated
    else Task.throw $ Exit.DetailsBadGrenInAppOutline grenVersion

-- VERIFY CONSTRAINTS

verifyConstraints ::
  Env ->
  Platform.Platform ->
  Map.Map Pkg.Name (PossibleFilePath Con.Constraint) ->
  Task (Map.Map Pkg.Name Solver.Details)
verifyConstraints (Env reportKey _ _ cache) rootPlatform constraints =
  do
    result <- Task.io $ Solver.verify reportKey cache rootPlatform constraints
    case result of
      Solver.Ok details -> return details
      Solver.NoSolution -> Task.throw Exit.DetailsNoSolution
      Solver.Err exit -> Task.throw $ Exit.DetailsSolverProblem exit

-- UNION

union :: (Ord k) => (k -> v -> v -> Task v) -> Map.Map k v -> Map.Map k v -> Task (Map.Map k v)
union tieBreaker deps1 deps2 =
  Map.mergeA Map.preserveMissing Map.preserveMissing (Map.zipWithAMatched tieBreaker) deps1 deps2

noDups :: Pkg.Name -> v -> v -> Task v
noDups depName _ _ =
  Task.throw $ Exit.DetailsDuplicatedDep depName

-- FORK

fork :: IO a -> IO (MVar a)
fork work =
  do
    mvar <- newEmptyMVar
    _ <- forkIO $ putMVar mvar =<< work
    return mvar

-- VERIFY DEPENDENCIES

verifyDependencies :: Env -> File.Time -> ValidOutline -> Map.Map Pkg.Name Solver.Details -> Map.Map Pkg.Name a -> Task Details
verifyDependencies env@(Env _ scope root _) time outline solution directDeps =
  Task.eio id $
    do
      mvar <- newEmptyMVar
      mvars <- Map.traverseWithKey (\k v -> fork (verifyDep env mvar solution k v)) solution
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
              details = Details time outline 0 Map.empty foreigns (ArtifactsFresh ifaces objs)
           in do
                BW.writeBinary scope (Dirs.objects root) objs
                BW.writeBinary scope (Dirs.interfaces root) ifaces
                BW.writeBinary scope (Dirs.details root) details
                return (Right details)

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

verifyDep :: Env -> MVar (Map.Map Pkg.Name (MVar Dep)) -> Map.Map Pkg.Name Solver.Details -> Pkg.Name -> Solver.Details -> IO Dep
verifyDep (Env key _ _ cache) depsMVar solution pkg details@(Solver.Details vsn maybeLocalPath directDeps) =
  do
    let fingerprint = Map.intersectionWith (\(Solver.Details v _ _) _ -> v) solution directDeps
    maybeCache <-
      case maybeLocalPath of
        Nothing ->
          File.readBinary (Dirs.package cache pkg vsn </> "artifacts.dat")
        Just _ ->
          return Nothing
    case maybeCache of
      Nothing ->
        build key cache depsMVar pkg details fingerprint Set.empty
      Just (ArtifactCache fingerprints artifacts) ->
        if Set.member fingerprint fingerprints
          then Reporting.report key Reporting.DBuilt >> return (Right artifacts)
          else build key cache depsMVar pkg details fingerprint fingerprints

-- ARTIFACT CACHE

data ArtifactCache = ArtifactCache
  { _fingerprints :: Set.Set Fingerprint,
    _artifacts :: Artifacts
  }

type Fingerprint =
  Map.Map Pkg.Name V.Version

-- BUILD

build :: Reporting.DKey -> Dirs.PackageCache -> MVar (Map.Map Pkg.Name (MVar Dep)) -> Pkg.Name -> Solver.Details -> Fingerprint -> Set.Set Fingerprint -> IO Dep
build key cache depsMVar pkg (Solver.Details vsn maybeLocalPath _) f fs =
  do
    let packageDir = Maybe.fromMaybe (Dirs.package cache pkg vsn) maybeLocalPath
    eitherOutline <- Outline.read packageDir
    case eitherOutline of
      Left _ ->
        do
          Reporting.report key Reporting.DBroken
          return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f
      Right (Outline.App _) ->
        do
          Reporting.report key Reporting.DBroken
          return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f
      Right (Outline.Pkg (Outline.PkgOutline _ _ _ _ exposed deps _ platform)) ->
        do
          allDeps <- readMVar depsMVar
          directDeps <- traverse readMVar (Map.intersection allDeps deps)
          case sequence directDeps of
            Left _ ->
              do
                Reporting.report key Reporting.DBroken
                return $ Left Nothing
            Right directArtifacts ->
              do
                let src = packageDir </> "src"
                let foreignDeps = gatherForeignInterfaces directArtifacts
                let exposedDict = Map.fromKeys (const ()) (Outline.flattenExposed exposed)
                docsStatus <- getDocsStatus packageDir
                authorizedForKernelCode <- packageAuthorizedForKernelCode pkg
                mvar <- newEmptyMVar
                mvars <- Map.traverseWithKey (const . fork . crawlModule foreignDeps mvar pkg src docsStatus authorizedForKernelCode) exposedDict
                putMVar mvar mvars
                mapM_ readMVar mvars
                maybeStatuses <- traverse readMVar =<< readMVar mvar
                case sequence maybeStatuses of
                  Left CrawlCorruption ->
                    do
                      Reporting.report key Reporting.DBroken
                      return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f
                  Left CrawlUnsignedKernelCode ->
                    do
                      Reporting.report key Reporting.DBroken
                      return $ Left $ Just $ Exit.BD_UnsignedBuild pkg vsn
                  Right statuses ->
                    do
                      rmvar <- newEmptyMVar
                      rmvars <- traverse (fork . compile platform pkg rmvar) statuses
                      putMVar rmvar rmvars
                      maybeResults <- traverse readMVar rmvars
                      case sequence maybeResults of
                        Nothing ->
                          do
                            Reporting.report key Reporting.DBroken
                            return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f
                        Just results ->
                          let path = packageDir </> "artifacts.dat"
                              ifaces = gatherInterfaces exposedDict results
                              objects = gatherObjects results
                              artifacts = Artifacts ifaces objects
                              fingerprints = Set.insert f fs
                           in if Maybe.isJust maybeLocalPath
                                then do
                                  Reporting.report key Reporting.DBuilt
                                  return (Right artifacts)
                                else do
                                  writeDocs packageDir docsStatus results
                                  File.writeBinary path (ArtifactCache fingerprints artifacts)
                                  Reporting.report key Reporting.DBuilt
                                  return (Right artifacts)

packageAuthorizedForKernelCode :: Pkg.Name -> IO Bool
packageAuthorizedForKernelCode pkg =
  return $ Pkg.isKernel pkg

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

crawlModule :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> Bool -> ModuleName.Raw -> IO (Either CrawlError Status)
crawlModule foreignDeps mvar pkg src docsStatus authorizedForKernelCode name =
  do
    let path = src </> ModuleName.toFilePath name <.> "gren"
    exists <- File.exists path
    case Map.lookup name foreignDeps of
      Just ForeignAmbiguous ->
        return $ Left CrawlCorruption
      Just (ForeignSpecific iface) ->
        if exists
          then return $ Left CrawlCorruption
          else return (Right (SForeign iface))
      Nothing ->
        if exists
          then crawlFile foreignDeps mvar pkg src docsStatus authorizedForKernelCode name path
          else
            if Pkg.isKernel pkg && Name.isKernel name
              then
                if authorizedForKernelCode
                  then crawlKernel foreignDeps mvar pkg src name
                  else return $ Left CrawlUnsignedKernelCode
              else return $ Left CrawlCorruption

crawlFile :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> Bool -> ModuleName.Raw -> FilePath -> IO (Either CrawlError Status)
crawlFile foreignDeps mvar pkg src docsStatus authorizedForKernelCode expectedName path =
  do
    bytes <- File.readUtf8 path
    case Parse.fromByteString (Parse.Package pkg) bytes of
      Right modul@(Src.Module (Just (A.At _ actualName)) _ _ imports _ _ _ _ _ _ _) | expectedName == actualName ->
        do
          deps <- crawlImports foreignDeps mvar pkg authorizedForKernelCode src (fmap snd imports)
          return (Right (SLocal docsStatus deps modul))
      _ ->
        return $ Left CrawlCorruption

crawlImports :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> Bool -> FilePath -> [Src.Import] -> IO (Map.Map ModuleName.Raw ())
crawlImports foreignDeps mvar pkg authorizedForKernelCode src imports =
  do
    statusDict <- takeMVar mvar
    let deps = Map.fromList (map (\i -> (Src.getImportName i, ())) imports)
    let news = Map.difference deps statusDict
    mvars <- Map.traverseWithKey (const . fork . crawlModule foreignDeps mvar pkg src DocsNotNeeded authorizedForKernelCode) news
    putMVar mvar (Map.union mvars statusDict)
    mapM_ readMVar mvars
    return deps

crawlKernel :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> ModuleName.Raw -> IO (Either CrawlError Status)
crawlKernel foreignDeps mvar pkg src name =
  do
    let path = src </> ModuleName.toFilePath name <.> "js"
    exists <- File.exists path
    if exists
      then do
        bytes <- File.readUtf8 path
        case Kernel.fromByteString pkg (Map.mapMaybe getDepHome foreignDeps) bytes of
          Nothing ->
            return $ Left CrawlCorruption
          Just (Kernel.Content imports chunks) ->
            do
              _ <- crawlImports foreignDeps mvar pkg True src imports
              return (Right (SKernelLocal chunks))
      else return (Right SKernelForeign)

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

getDocsStatus :: FilePath -> IO DocsStatus
getDocsStatus packageDir =
  do
    exists <- File.exists (packageDir </> "docs.json")
    if exists
      then return DocsNotNeeded
      else return DocsNeeded

makeDocs :: DocsStatus -> Can.Module -> Maybe Docs.Module
makeDocs status modul =
  case status of
    DocsNeeded ->
      case Docs.fromModule modul of
        Right docs -> Just docs
        Left _ -> Nothing
    DocsNotNeeded ->
      Nothing

writeDocs :: FilePath -> DocsStatus -> Map.Map ModuleName.Raw Result -> IO ()
writeDocs packageDir status results =
  case status of
    DocsNeeded ->
      E.writeUgly (packageDir </> "docs.json") $
        Docs.encode $
          Map.mapMaybe toDocs results
    DocsNotNeeded ->
      return ()

toDocs :: Result -> Maybe Docs.Module
toDocs result =
  case result of
    RLocal _ _ docs -> docs
    RForeign _ -> Nothing
    RKernelLocal _ -> Nothing
    RKernelForeign -> Nothing

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
  put (Local a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get =
    do
      a <- get
      b <- get
      c <- get
      d <- get
      e <- get
      f <- get
      return (Local a b c d e f)

instance Binary Foreign where
  get = liftM2 Foreign get get
  put (Foreign a b) = put a >> put b

instance Binary Artifacts where
  get = liftM2 Artifacts get get
  put (Artifacts a b) = put a >> put b

instance Binary ArtifactCache where
  get = liftM2 ArtifactCache get get
  put (ArtifactCache a b) = put a >> put b
