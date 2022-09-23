{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Deps.Solver
  ( Solver,
    Result (..),
    --
    Details (..),
    verify,
    --
    AppSolution (..),
    addToApp,
    --
    Env (..),
    initEnv,
  )
where

import Control.Monad (foldM)
import Data.Map ((!))
import Data.Map qualified as Map
import Deps.Package qualified as Package
import Directories qualified as Dirs
import File qualified
import Gren.Constraint qualified as C
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Gren.Platform qualified as Platform
import Gren.Version qualified as V
import Json.Decode qualified as D
import Reporting.Exit qualified as Exit
import System.FilePath ((</>))

-- SOLVER

newtype Solver a
  = Solver
      ( forall b.
        State ->
        (State -> a -> (State -> IO b) -> IO b) ->
        (State -> IO b) ->
        (Exit.Solver -> IO b) ->
        IO b
      )

data State = State
  { _cache :: Dirs.PackageCache,
    _constraints :: Map.Map (Pkg.Name, V.Version) Constraints
  }

data Constraints = Constraints
  { _gren :: C.Constraint,
    _platform :: Platform.Platform,
    _deps :: Map.Map Pkg.Name C.Constraint
  }

-- RESULT

data Result a
  = Ok a
  | NoSolution
  | Err Exit.Solver

-- VERIFY -- used by Gren.Details

data Details
  = Details V.Version (Map.Map Pkg.Name C.Constraint)

verify ::
  Dirs.PackageCache ->
  Platform.Platform ->
  Map.Map Pkg.Name C.Constraint ->
  IO (Result (Map.Map Pkg.Name Details))
verify cache rootPlatform constraints =
  Dirs.withRegistryLock cache $
    case try rootPlatform constraints of
      Solver solver ->
        solver
          (State cache Map.empty)
          (\s a _ -> return $ Ok (Map.mapWithKey (addDeps s) a))
          (\_ -> return NoSolution)
          (\e -> return $ Err e)

addDeps :: State -> Pkg.Name -> V.Version -> Details
addDeps (State _ constraints) name vsn =
  case Map.lookup (name, vsn) constraints of
    Just (Constraints _ _ deps) -> Details vsn deps
    Nothing -> error "compiler bug manifesting in Deps.Solver.addDeps"

-- ADD TO APP - used in Install

data AppSolution = AppSolution
  { _old :: Map.Map Pkg.Name V.Version,
    _new :: Map.Map Pkg.Name V.Version,
    _app :: Outline.AppOutline
  }

addToApp ::
  Dirs.PackageCache ->
  Pkg.Name ->
  V.Version ->
  Outline.AppOutline ->
  IO (Result AppSolution)
addToApp cache pkg compatibleVsn outline@(Outline.AppOutline _ rootPlatform _ direct indirect) =
  Dirs.withRegistryLock cache $
    let allDeps = Map.union direct indirect

        attempt toConstraint deps =
          try
            rootPlatform
            (Map.insert pkg (C.untilNextMajor compatibleVsn) (Map.map toConstraint deps))
     in case oneOf
          (attempt C.exactly allDeps)
          [ attempt C.exactly direct,
            attempt C.untilNextMinor direct,
            attempt C.untilNextMajor direct
          ] of
          Solver solver ->
            solver
              (State cache Map.empty)
              (\s a _ -> return $ Ok (toApp s pkg outline allDeps a))
              (\_ -> return $ NoSolution)
              (\e -> return $ Err e)

toApp :: State -> Pkg.Name -> Outline.AppOutline -> Map.Map Pkg.Name V.Version -> Map.Map Pkg.Name V.Version -> AppSolution
toApp (State _ constraints) pkg (Outline.AppOutline gren platform srcDirs direct _) old new =
  let d = Map.intersection new (Map.insert pkg V.one direct)
      i = Map.difference (getTransitive constraints new (Map.toList d) Map.empty) d
   in AppSolution old new (Outline.AppOutline gren platform srcDirs d i)

getTransitive :: Map.Map (Pkg.Name, V.Version) Constraints -> Map.Map Pkg.Name V.Version -> [(Pkg.Name, V.Version)] -> Map.Map Pkg.Name V.Version -> Map.Map Pkg.Name V.Version
getTransitive constraints solution unvisited visited =
  case unvisited of
    [] ->
      visited
    info@(pkg, vsn) : infos ->
      if Map.member pkg visited
        then getTransitive constraints solution infos visited
        else
          let newDeps = _deps (constraints ! info)
              newUnvisited = Map.toList (Map.intersection solution (Map.difference newDeps visited))
              newVisited = Map.insert pkg vsn visited
           in getTransitive constraints solution infos $
                getTransitive constraints solution newUnvisited newVisited

-- TRY

try :: Platform.Platform -> Map.Map Pkg.Name C.Constraint -> Solver (Map.Map Pkg.Name V.Version)
try rootPlatform constraints =
  exploreGoals (Goals rootPlatform constraints Map.empty)

-- EXPLORE GOALS

data Goals = Goals
  { _root_platform :: Platform.Platform,
    _pending :: Map.Map Pkg.Name C.Constraint,
    _solved :: Map.Map Pkg.Name V.Version
  }

exploreGoals :: Goals -> Solver (Map.Map Pkg.Name V.Version)
exploreGoals (Goals rootPlatform pending solved) =
  case Map.minViewWithKey pending of
    Nothing ->
      return solved
    Just ((name, constraint), otherPending) ->
      do
        let goals1 = Goals rootPlatform otherPending solved
        let lowestVersion = C.lowerBound constraint
        goals2 <- addVersion goals1 name lowestVersion
        exploreGoals goals2

addVersion :: Goals -> Pkg.Name -> V.Version -> Solver Goals
addVersion (Goals rootPlatform pending solved) name version =
  do
    (Constraints gren platform deps) <- getConstraints name version
    if C.goodGren gren && Platform.compatible rootPlatform platform
      then do
        newPending <- foldM (addConstraint name solved) pending (Map.toList deps)
        return (Goals rootPlatform newPending (Map.insert name version solved))
      else backtrack

addConstraint :: Pkg.Name -> Map.Map Pkg.Name V.Version -> Map.Map Pkg.Name C.Constraint -> (Pkg.Name, C.Constraint) -> Solver (Map.Map Pkg.Name C.Constraint)
addConstraint sourcePkg solved unsolved (name, newConstraint) =
  case Map.lookup name solved of
    Just version ->
      if C.satisfies newConstraint version
        then return unsolved
        else
          solverError $
            Exit.SolverIncompatibleSolvedVersion sourcePkg name newConstraint version
    Nothing ->
      case Map.lookup name unsolved of
        Nothing ->
          return $ Map.insert name newConstraint unsolved
        Just oldConstraint ->
          case C.intersect oldConstraint newConstraint of
            Nothing ->
              solverError $
                Exit.SolverIncompatibleVersionRanges sourcePkg name oldConstraint newConstraint
            Just mergedConstraint ->
              if oldConstraint == mergedConstraint
                then return unsolved
                else return (Map.insert name mergedConstraint unsolved)

-- GET CONSTRAINTS

getConstraints :: Pkg.Name -> V.Version -> Solver Constraints
getConstraints pkg vsn =
  Solver $ \state@(State cache cDict) ok back err ->
    do
      let key = (pkg, vsn)
      case Map.lookup key cDict of
        Just cs ->
          ok state cs back
        Nothing ->
          do
            let toNewState cs = State cache (Map.insert key cs cDict)
            let home = Dirs.package cache pkg vsn
            packageInstalResult <- Package.installPackageVersion cache pkg vsn
            case packageInstalResult of
              Left gitErr ->
                err $ Exit.SolverBadGitOperationVersionedPkg pkg vsn gitErr
              Right () -> do
                let path = home </> "gren.json"
                outlineExists <- File.exists path
                if outlineExists
                  then do
                    bytes <- File.readUtf8 path
                    case D.fromByteString constraintsDecoder bytes of
                      Right cs ->
                        ok (toNewState cs) cs back
                      Left _ ->
                        err (Exit.SolverBadCacheData pkg vsn)
                  else err (Exit.SolverBadCacheData pkg vsn)

constraintsDecoder :: D.Decoder () Constraints
constraintsDecoder =
  do
    outline <- D.mapError (const ()) Outline.decoder
    case outline of
      Outline.Pkg (Outline.PkgOutline _ _ _ _ _ deps grenConstraint platform) ->
        return (Constraints grenConstraint platform deps)
      Outline.App _ ->
        D.failure ()

-- ENVIRONMENT

newtype Env
  = Env Dirs.PackageCache

initEnv :: IO Env
initEnv =
  do
    cache <- Dirs.getPackageCache
    return $ Env cache

-- INSTANCES

instance Functor Solver where
  fmap func (Solver solver) =
    Solver $ \state ok back err ->
      let okA stateA arg backA = ok stateA (func arg) backA
       in solver state okA back err

instance Applicative Solver where
  pure a =
    Solver $ \state ok back _ -> ok state a back

  (<*>) (Solver solverFunc) (Solver solverArg) =
    Solver $ \state ok back err ->
      let okF stateF func backF =
            let okA stateA arg backA = ok stateA (func arg) backA
             in solverArg stateF okA backF err
       in solverFunc state okF back err

instance Monad Solver where
  (>>=) (Solver solverA) callback =
    Solver $ \state ok back err ->
      let okA stateA a backA =
            case callback a of
              Solver solverB -> solverB stateA ok backA err
       in solverA state okA back err

oneOf :: Solver a -> [Solver a] -> Solver a
oneOf solver@(Solver solverHead) solvers =
  case solvers of
    [] ->
      solver
    s : ss ->
      Solver $ \state0 ok back err ->
        let tryTail state1 =
              let (Solver solverTail) = oneOf s ss
               in solverTail state1 ok back err
         in solverHead state0 ok tryTail err

backtrack :: Solver a
backtrack =
  Solver $ \state _ back _ -> back state

solverError :: Exit.Solver -> Solver a
solverError errorState =
  Solver $ \_ _ _ err -> err errorState
