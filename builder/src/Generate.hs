module Generate
  ( dev,
    prod,
    repl,
  )
where

import AST.Optimized qualified as Opt
import Build qualified
import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Monad (liftM2)
import Data.ByteString.Builder qualified as B
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Name qualified as N
import Data.NonEmptyList qualified as NE
import Directories qualified as Dirs
import File qualified
import Generate.JavaScript qualified as JS
import Generate.Mode qualified as Mode
import Gren.Details qualified as Details
import Gren.ModuleName qualified as ModuleName
import Gren.Package qualified as Pkg
import Nitpick.Debug qualified as Nitpick
import Reporting.Exit qualified as Exit
import Reporting.Task qualified as Task
import Prelude hiding (cycle, print)

-- GENERATORS

type Task a =
  Task.Task Exit.Generate a

dev :: FilePath -> Details.Details -> Build.Artifacts -> Task JS.GeneratedResult
dev root details (Build.Artifacts pkg _ roots modules) =
  do
    objects <- finalizeObjects =<< loadObjects root details modules
    let mode = Mode.Dev
    let graph = objectsToGlobalGraph objects
    let mains = gatherMains pkg objects roots
    return $ JS.generate mode graph mains

prod :: FilePath -> Details.Details -> Build.Artifacts -> Task JS.GeneratedResult
prod root details (Build.Artifacts pkg _ roots modules) =
  do
    objects <- finalizeObjects =<< loadObjects root details modules
    checkForDebugUses objects
    let graph = objectsToGlobalGraph objects
    let mode = Mode.Prod (Mode.shortenFieldNames graph)
    let mains = gatherMains pkg objects roots
    return $ JS.generate mode graph mains

repl :: FilePath -> Details.Details -> Bool -> Build.ReplArtifacts -> N.Name -> Task B.Builder
repl root details ansi (Build.ReplArtifacts home modules localizer annotations) name =
  do
    objects <- finalizeObjects =<< loadObjects root details modules
    let graph = objectsToGlobalGraph objects
    return $ JS.generateForRepl ansi localizer graph home name (annotations ! name)

-- CHECK FOR DEBUG

checkForDebugUses :: Objects -> Task ()
checkForDebugUses (Objects _ locals) =
  case Map.keys (Map.filter Nitpick.hasDebugUses locals) of
    [] -> return ()
    m : ms -> Task.throw (Exit.GenerateCannotOptimizeDebugValues m ms)

-- GATHER MAINS

gatherMains :: Pkg.Name -> Objects -> NE.List Build.Root -> Map.Map ModuleName.Canonical Opt.Main
gatherMains pkg (Objects _ locals) roots =
  Map.fromList $ Maybe.mapMaybe (lookupMain pkg locals) (NE.toList roots)

lookupMain :: Pkg.Name -> Map.Map ModuleName.Raw Opt.LocalGraph -> Build.Root -> Maybe (ModuleName.Canonical, Opt.Main)
lookupMain pkg locals root =
  let toPair name (Opt.LocalGraph maybeMain _ _) =
        (,) (ModuleName.Canonical pkg name) <$> maybeMain
   in case root of
        Build.Inside name -> toPair name =<< Map.lookup name locals
        Build.Outside name _ g -> toPair name g

-- LOADING OBJECTS

data LoadingObjects = LoadingObjects
  { _foreign_mvar :: MVar (Maybe Opt.GlobalGraph),
    _local_mvars :: Map.Map ModuleName.Raw (MVar (Maybe Opt.LocalGraph))
  }

loadObjects :: FilePath -> Details.Details -> [Build.Module] -> Task LoadingObjects
loadObjects root details modules =
  Task.io $
    do
      mvar <- Details.loadObjects root details
      mvars <- traverse (loadObject root) modules
      return $ LoadingObjects mvar (Map.fromList mvars)

loadObject :: FilePath -> Build.Module -> IO (ModuleName.Raw, MVar (Maybe Opt.LocalGraph))
loadObject root modul =
  case modul of
    Build.Fresh name _ graph ->
      do
        mvar <- newMVar (Just graph)
        return (name, mvar)
    Build.Cached name _ _ ->
      do
        mvar <- newEmptyMVar
        _ <- forkIO $ putMVar mvar =<< File.readBinary (Dirs.greno root name)
        return (name, mvar)

-- FINALIZE OBJECTS

data Objects = Objects
  { _foreign :: Opt.GlobalGraph,
    _locals :: Map.Map ModuleName.Raw Opt.LocalGraph
  }

finalizeObjects :: LoadingObjects -> Task Objects
finalizeObjects (LoadingObjects mvar mvars) =
  Task.eio id $
    do
      result <- readMVar mvar
      results <- traverse readMVar mvars
      case liftM2 Objects result (sequence results) of
        Just loaded -> return (Right loaded)
        Nothing -> return (Left Exit.GenerateCannotLoadArtifacts)

objectsToGlobalGraph :: Objects -> Opt.GlobalGraph
objectsToGlobalGraph (Objects globals locals) =
  foldr Opt.addLocalGraph globals locals
