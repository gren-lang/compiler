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
import Data.Maybe qualified as Maybe
import Deps.Package qualified as Package
import Directories qualified as Dirs
import File qualified
import Gren.Constraint qualified as C
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Gren.Platform qualified as Platform
import Gren.PossibleFilePath (PossibleFilePath)
import Gren.PossibleFilePath qualified as PossibleFilePath
import Gren.Version qualified as V
import Json.Decode qualified as D
import Reporting qualified
import Reporting.Exit qualified as Exit
import System.Directory qualified as Dir
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
    _deps :: Map.Map Pkg.Name (PossibleFilePath C.Constraint)
  }

-- RESULT

data Result a
  = Ok a
  | NoSolution
  | Err Exit.Solver

-- VERIFY -- used by Gren.Details

data Details
  = Details V.Version (Maybe FilePath) (Map.Map Pkg.Name (PossibleFilePath C.Constraint))

verify ::
  Reporting.DKey ->
  Dirs.PackageCache ->
  Platform.Platform ->
  Map.Map Pkg.Name (PossibleFilePath C.Constraint) ->
  IO (Result (Map.Map Pkg.Name Details))
verify key cache rootPlatform constraints =
  Dirs.withRegistryLock cache $
    case try key rootPlatform constraints of
      Solver solver ->
        solver
          (State cache Map.empty)
          (\s a _ -> return $ Ok (Map.mapWithKey (addDeps s) a))
          (\_ -> return NoSolution)
          (\e -> return $ Err e)

addDeps :: State -> Pkg.Name -> ConstraintSource -> Details
addDeps (State _ constraints) name constraintSource =
  let vsn = C.lowerBound $ constraintFromCS constraintSource
   in case Map.lookup (name, vsn) constraints of
        Just (Constraints _ _ deps) -> Details vsn (filePathFromCS constraintSource) deps
        Nothing -> error "compiler bug manifesting in Deps.Solver.addDeps"

-- ADD TO APP - used in Install

data AppSolution = AppSolution
  { _old :: Map.Map Pkg.Name (PossibleFilePath V.Version),
    _new :: Map.Map Pkg.Name (PossibleFilePath V.Version),
    _app :: Outline.AppOutline
  }

addToApp ::
  Reporting.DKey ->
  Dirs.PackageCache ->
  Pkg.Name ->
  V.Version ->
  Outline.AppOutline ->
  IO (Result AppSolution)
addToApp key cache pkg compatibleVsn outline@(Outline.AppOutline _ rootPlatform _ direct indirect) =
  Dirs.withRegistryLock cache $
    let allDeps = Map.union direct indirect

        insertableVsn = PossibleFilePath.Other (C.untilNextMajor compatibleVsn)

        attempt toConstraint deps =
          try
            key
            rootPlatform
            (Map.insert pkg insertableVsn (Map.map (PossibleFilePath.mapWith toConstraint) deps))
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

toApp :: State -> Pkg.Name -> Outline.AppOutline -> Map.Map Pkg.Name (PossibleFilePath V.Version) -> Map.Map Pkg.Name ConstraintSource -> AppSolution
toApp (State _ constraints) pkg (Outline.AppOutline gren platform srcDirs direct _) old new =
  let newAsPFPs = Map.map constraintToFilePath new
      d = Map.intersection newAsPFPs (Map.insert pkg (PossibleFilePath.Other V.one) direct)
      dCSs = filter (\(pkgName, _) -> Map.member pkgName d) $ Map.toList new
      i = Map.map constraintToFilePath $ Map.difference (getTransitive constraints new dCSs Map.empty) d
   in AppSolution old newAsPFPs (Outline.AppOutline gren platform srcDirs d i)

constraintToFilePath :: ConstraintSource -> PossibleFilePath V.Version
constraintToFilePath cs =
  case cs of
    Local _ fp -> PossibleFilePath.Is fp
    Remote con -> PossibleFilePath.Other $ C.lowerBound con

getTransitive :: Map.Map (Pkg.Name, V.Version) Constraints -> Map.Map Pkg.Name ConstraintSource -> [(Pkg.Name, ConstraintSource)] -> Map.Map Pkg.Name ConstraintSource -> Map.Map Pkg.Name ConstraintSource
getTransitive constraints solution unvisited visited =
  case unvisited of
    [] ->
      visited
    (pkg, cs) : infos ->
      if Map.member pkg visited
        then getTransitive constraints solution infos visited
        else
          let vsn = C.lowerBound $ constraintFromCS cs
              newDeps = _deps (constraints ! (pkg, vsn))
              newUnvisited = Map.toList (Map.intersection solution (Map.difference newDeps visited))
              newVisited = Map.insert pkg cs visited
           in getTransitive constraints solution infos $
                getTransitive constraints solution newUnvisited newVisited

-- CONSTRAINT SOURCE

data ConstraintSource
  = Remote C.Constraint
  | Local C.Constraint FilePath

-- TODO: Avoid re-reading the gren.json for local dependencies
resolveToConstraintSource :: Pkg.Name -> PossibleFilePath C.Constraint -> Solver ConstraintSource
resolveToConstraintSource expectedPkgName possibleFP =
  Solver $ \state ok back err ->
    case possibleFP of
      PossibleFilePath.Other cons ->
        ok state (Remote cons) back
      PossibleFilePath.Is fp ->
        do
          outlineExists <- Dir.doesDirectoryExist fp
          if outlineExists
            then do
              let outlinePath = fp </> "gren.json"
              bytes <- File.readUtf8 outlinePath
              case D.fromByteString Outline.decoder bytes of
                Right (Outline.Pkg (Outline.PkgOutline pkgName _ _ version _ _ _ _)) ->
                  if expectedPkgName /= pkgName
                    then err $ Exit.SolverBadLocalDepWrongName fp expectedPkgName pkgName
                    else ok state (Local (C.exactly version) fp) back
                Right (Outline.App _) ->
                  err $ Exit.SolverBadLocalDepExpectedPkg fp expectedPkgName
                Left _ ->
                  err $ Exit.SolverBadLocalDepInvalidGrenJson fp expectedPkgName
            else err $ Exit.SolverLocalDepNotFound fp expectedPkgName

constraintFromCS :: ConstraintSource -> C.Constraint
constraintFromCS source =
  case source of
    Remote c -> c
    Local c _ -> c

setConstraintInCS :: C.Constraint -> ConstraintSource -> ConstraintSource
setConstraintInCS newCons source =
  case source of
    Remote _ -> Remote newCons
    Local _ fp -> Local newCons fp

filePathFromCS :: ConstraintSource -> Maybe FilePath
filePathFromCS source =
  case source of
    Remote _ -> Nothing
    Local _ fp -> Just fp

-- TRY

try :: Reporting.DKey -> Platform.Platform -> Map.Map Pkg.Name (PossibleFilePath C.Constraint) -> Solver (Map.Map Pkg.Name ConstraintSource)
try key rootPlatform constraints =
  do
    constraintSources <- Map.traverseWithKey resolveToConstraintSource constraints
    exploreGoals key (Goals rootPlatform constraintSources Map.empty)

-- EXPLORE GOALS

data Goals = Goals
  { _root_platform :: Platform.Platform,
    _pending :: Map.Map Pkg.Name ConstraintSource,
    _solved :: Map.Map Pkg.Name ConstraintSource
  }

exploreGoals :: Reporting.DKey -> Goals -> Solver (Map.Map Pkg.Name ConstraintSource)
exploreGoals key (Goals rootPlatform pending solved) =
  case Map.minViewWithKey pending of
    Nothing ->
      return solved
    Just ((name, constraintSource), otherPending) ->
      do
        let goals1 = Goals rootPlatform otherPending solved
        goals2 <- addVersion key goals1 name constraintSource
        exploreGoals key goals2

addVersion :: Reporting.DKey -> Goals -> Pkg.Name -> ConstraintSource -> Solver Goals
addVersion reportKey (Goals rootPlatform pending solved) name source =
  do
    let constraint = constraintFromCS source
    let lowestVersion = C.lowerBound constraint
    let maybeFilePath = filePathFromCS source
    (Constraints gren platform deps) <- getConstraints reportKey name lowestVersion maybeFilePath
    if C.goodGren gren
      then
        if Platform.compatible rootPlatform platform
          then do
            depsConstraintSources <- Map.traverseWithKey resolveToConstraintSource deps
            newPending <- foldM (addConstraint name solved) pending (Map.toList depsConstraintSources)
            return (Goals rootPlatform newPending (Map.insert name source solved))
          else
            solverError $
              Exit.SolverIncompatiblePlatforms name rootPlatform platform
      else backtrack

addConstraint :: Pkg.Name -> Map.Map Pkg.Name ConstraintSource -> Map.Map Pkg.Name ConstraintSource -> (Pkg.Name, ConstraintSource) -> Solver (Map.Map Pkg.Name ConstraintSource)
addConstraint sourcePkg solved unsolved (name, newConstraintSource) =
  let newConstraint = constraintFromCS newConstraintSource
   in case Map.lookup name solved of
        Just solvedConstraintSource ->
          if not $ compatibleConstraintSources solvedConstraintSource newConstraintSource
            then
              solverError $
                Exit.SolverTransientLocalDep sourcePkg name
            else
              let solvedVersion = C.lowerBound $ constraintFromCS solvedConstraintSource
               in if C.satisfies newConstraint solvedVersion
                    then return unsolved
                    else
                      solverError $
                        Exit.SolverIncompatibleSolvedVersion sourcePkg name newConstraint solvedVersion
        Nothing ->
          case Map.lookup name unsolved of
            Nothing ->
              return $ Map.insert name newConstraintSource unsolved
            Just oldConstraintSource ->
              if not $ compatibleConstraintSources oldConstraintSource newConstraintSource
                then
                  solverError $
                    Exit.SolverTransientLocalDep sourcePkg name
                else
                  let oldConstraint = constraintFromCS oldConstraintSource
                   in case C.intersect oldConstraint newConstraint of
                        Nothing ->
                          solverError $
                            Exit.SolverIncompatibleVersionRanges sourcePkg name oldConstraint newConstraint
                        Just mergedConstraint ->
                          if oldConstraint == mergedConstraint
                            then return unsolved
                            else return (Map.insert name (setConstraintInCS mergedConstraint newConstraintSource) unsolved)

compatibleConstraintSources :: ConstraintSource -> ConstraintSource -> Bool
compatibleConstraintSources a b =
  case (a, b) of
    (Local _ aPath, Local _ bPath) ->
      aPath == bPath
    (Remote _, Remote _) ->
      True
    (Remote _, Local _ _) ->
      False
    (Local _ _, Remote _) ->
      -- Application is allowed to override
      True

-- GET CONSTRAINTS

getConstraints :: Reporting.DKey -> Pkg.Name -> V.Version -> Maybe FilePath -> Solver Constraints
getConstraints reportKey pkg vsn maybeFilePath =
  Solver $ \state@(State cache cDict) ok back err ->
    do
      let key = (pkg, vsn)
      case Map.lookup key cDict of
        Just cs ->
          ok state cs back
        Nothing ->
          do
            let packageCachePath = Dirs.package cache pkg vsn
            let path = Maybe.fromMaybe packageCachePath maybeFilePath
            isPackageOnDisk <- Dir.doesDirectoryExist path
            if isPackageOnDisk
              then do
                Reporting.report reportKey Reporting.DCached
                constraintsDecodeResult <- getConstraintsHelper path pkg vsn
                case constraintsDecodeResult of
                  Left exitMsg ->
                    err exitMsg
                  Right cs ->
                    ok (State cache (Map.insert key cs cDict)) cs back
              else do
                Reporting.report reportKey Reporting.DRequested
                packageInstalResult <- Package.installPackageVersion cache pkg vsn
                case packageInstalResult of
                  Left gitErr ->
                    do
                      Reporting.report reportKey $ Reporting.DFailed pkg vsn
                      err $ Exit.SolverBadGitOperationVersionedPkg pkg vsn gitErr
                  Right () -> do
                    Reporting.report reportKey $ Reporting.DReceived pkg vsn
                    constraintsDecodeResult <- getConstraintsHelper packageCachePath pkg vsn
                    case constraintsDecodeResult of
                      Left exitMsg ->
                        err exitMsg
                      Right cs ->
                        ok (State cache (Map.insert key cs cDict)) cs back

getConstraintsHelper :: FilePath -> Pkg.Name -> V.Version -> IO (Either Exit.Solver Constraints)
getConstraintsHelper projectRoot pkg vsn =
  do
    let path = projectRoot </> "gren.json"
    bytes <- File.readUtf8 path
    case D.fromByteString constraintsDecoder bytes of
      Right cs ->
        return $ Right cs
      Left _ ->
        return $ Left $ Exit.SolverBadCacheData pkg vsn

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
