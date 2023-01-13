{-# LANGUAGE OverloadedStrings #-}

module Init
  ( Flags (..),
    run,
  )
where

import Data.Map qualified as Map
import Data.NonEmptyList qualified as NE
import Deps.Package qualified as DPkg
import Deps.Solver qualified as Solver
import Directories qualified as Dirs
import Gren.Constraint qualified as Con
import Gren.Licenses qualified as Licenses
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Gren.Platform qualified as Platform
import Gren.PossibleFilePath (PossibleFilePath)
import Gren.PossibleFilePath qualified as PossibleFilePath
import Gren.Version qualified as V
import Reporting qualified
import Reporting.Doc qualified as D
import Reporting.Exit qualified as Exit
import System.Directory qualified as Dir
import Prelude hiding (init)

data Flags = Flags
  { _isPackage :: Bool,
    _platform :: Maybe Platform.Platform
  }

-- RUN

run :: () -> Flags -> IO ()
run () flags =
  Reporting.attempt Exit.initToReport $
    do
      exists <- Dir.doesFileExist "gren.json"
      if exists
        then return (Left Exit.InitAlreadyExists)
        else do
          approved <- Reporting.ask question
          if approved
            then init flags
            else do
              putStrLn "Okay, I did not make any changes!"
              return (Right ())

question :: D.Doc
question =
  D.stack
    [ D.fillSep
        [ "Hello!",
          "Gren",
          "projects",
          "always",
          "start",
          "with",
          "an",
          D.green "gren.json",
          "file.",
          "I",
          "can",
          "create",
          "them!"
        ],
      "Would you like me to create an gren.json file now? [Y/n]: "
    ]

-- INIT

init :: Flags -> IO (Either Exit.Init ())
init flags =
  do
    let platform = selectPlatform flags
    let initialDeps = suggestDependencies platform
    (Solver.Env cache) <- Solver.initEnv
    potentialDeps <-
      Dirs.withRegistryLock cache $
        DPkg.latestCompatibleVersionForPackages cache initialDeps
    case potentialDeps of
      Left DPkg.NoCompatiblePackage ->
        return $ Left $ Exit.InitNoCompatibleDependencies Nothing
      Left (DPkg.GitError gitError) ->
        return $ Left $ Exit.InitNoCompatibleDependencies $ Just gitError
      Right resolvedDeps -> do
        let deps = Map.map PossibleFilePath.Other resolvedDeps
        result <- Solver.verify Reporting.ignorer cache platform deps
        case result of
          Solver.Err exit ->
            return (Left (Exit.InitSolverProblem exit))
          Solver.NoSolution ->
            return (Left (Exit.InitNoSolution initialDeps))
          Solver.Ok details ->
            let outline =
                  if _isPackage flags
                    then pkgOutline platform deps
                    else appOutlineFromSolverDetails platform initialDeps details
             in do
                  Dir.createDirectoryIfMissing True "src"
                  Outline.write "." outline
                  putStrLn "Okay, I created it."
                  return (Right ())

pkgOutline :: Platform.Platform -> Map.Map Pkg.Name (PossibleFilePath Con.Constraint) -> Outline.Outline
pkgOutline platform deps =
  Outline.Pkg $
    Outline.PkgOutline
      Pkg.dummyName
      Outline.defaultSummary
      Licenses.bsd3
      V.one
      (Outline.ExposedList [])
      deps
      Con.defaultGren
      platform

appOutlineFromSolverDetails ::
  Platform.Platform ->
  [Pkg.Name] ->
  Map.Map Pkg.Name Solver.Details ->
  Outline.Outline
appOutlineFromSolverDetails platform initialDeps details =
  let solution = Map.map (\(Solver.Details vsn _ _) -> vsn) details
      defaultDeps = Map.fromList $ map (\dep -> (dep, Con.exactly V.one)) initialDeps
      directs = Map.intersection solution defaultDeps
      indirects = Map.difference solution defaultDeps
   in Outline.App $
        Outline.AppOutline
          V.compiler
          platform
          (NE.List (Outline.RelativeSrcDir "src") [])
          (Map.map PossibleFilePath.Other directs)
          (Map.map PossibleFilePath.Other indirects)

selectPlatform :: Flags -> Platform.Platform
selectPlatform flags =
  case (_isPackage flags, _platform flags) of
    (True, Nothing) -> Platform.Common
    (False, Nothing) -> Platform.Browser
    (_, Just platform) -> platform

suggestDependencies :: Platform.Platform -> [Pkg.Name]
suggestDependencies platform =
  case platform of
    Platform.Common -> [Pkg.core]
    Platform.Browser -> [Pkg.core, Pkg.browser]
    Platform.Node -> [Pkg.core, Pkg.node]
