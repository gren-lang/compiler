{-# LANGUAGE OverloadedStrings #-}

module Init
  ( Flags (..),
    run,
  )
where

import Data.Either qualified as Either
import Data.Map qualified as Map
import Data.NonEmptyList qualified as NE
import Deps.Package qualified as DPkg
import Deps.Solver qualified as Solver
import Directories qualified as Dirs
import Gren.Constraint qualified as Con
import Gren.Licenses qualified as Licenses
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import Json.String qualified as Json
import Reporting qualified
import Reporting.Doc qualified as D
import Reporting.Exit qualified as Exit
import System.Directory qualified as Dir
import Prelude hiding (init)

data Flags = Flags
  { _isPackage :: Bool
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
    let initialDeps =
          if _isPackage flags
            then pkgDefaultDeps
            else appDefaultDeps
    (Solver.Env cache) <- Solver.initEnv
    depsM <- mapM (compatibleDependencies cache) initialDeps
    let deps = Map.fromList $ Either.rights depsM
    result <- Solver.verify cache deps
    case result of
      Solver.Err exit ->
        return (Left (Exit.InitSolverProblem exit))
      Solver.NoSolution ->
        return (Left (Exit.InitNoSolution initialDeps))
      Solver.NoOfflineSolution ->
        return (Left (Exit.InitNoOfflineSolution initialDeps))
      Solver.Ok details ->
        let outline =
              if _isPackage flags
                then pkgOutline deps
                else appOutlineFromSolverDetails details
         in do
              Dir.createDirectoryIfMissing True "src"
              Outline.write "." outline
              putStrLn "Okay, I created it."
              return (Right ())

compatibleDependencies :: Dirs.PackageCache -> Pkg.Name -> IO (Either () (Pkg.Name, Con.Constraint))
compatibleDependencies cache pkgName = do
  possibleVersion <- DPkg.getLatestCompatibleVersion cache pkgName
  case possibleVersion of
    Left _ ->
      return $ Left ()
    Right vsn ->
      return $ Right (pkgName, Con.untilNextMajor vsn)

pkgOutline :: Map.Map Pkg.Name Con.Constraint -> Outline.Outline
pkgOutline deps =
  Outline.Pkg $
    Outline.PkgOutline
      Pkg.dummyName
      (Json.fromChars "")
      Licenses.bsd3
      V.one
      (Outline.ExposedList [])
      deps
      Map.empty
      Con.defaultGren

appOutlineFromSolverDetails :: (Map.Map Pkg.Name Solver.Details) -> Outline.Outline
appOutlineFromSolverDetails details =
  let solution = Map.map (\(Solver.Details vsn _) -> vsn) details
      defaultDeps = Map.fromList $ map (\dep -> (dep, Con.anything)) appDefaultDeps
      directs = Map.intersection solution defaultDeps
      indirects = Map.difference solution defaultDeps
   in Outline.App $
        Outline.AppOutline
          V.compiler
          (NE.List (Outline.RelativeSrcDir "src") [])
          directs
          indirects
          Map.empty
          Map.empty

appDefaultDeps :: [Pkg.Name]
appDefaultDeps =
  [ Pkg.core,
    Pkg.browser
  ]

pkgDefaultDeps :: [Pkg.Name]
pkgDefaultDeps =
  [ Pkg.core
  ]
