{-# LANGUAGE OverloadedStrings #-}

module Init
  ( Flags (..),
    run,
  )
where

import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified Deps.Solver as Solver
import qualified Gren.Constraint as Con
import qualified Gren.Licenses as Licenses
import qualified Gren.Outline as Outline
import qualified Gren.Package as Pkg
import qualified Gren.Version as V
import qualified Json.String as Json
import qualified Reporting
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir
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
    let deps =
          if _isPackage flags
            then pkgDefaultDeps
            else appDefaultDeps
    (Solver.Env cache) <- Solver.initEnv
    result <- Solver.verify cache deps
    case result of
      Solver.Err exit ->
        return (Left (Exit.InitSolverProblem exit))
      Solver.NoSolution ->
        return (Left (Exit.InitNoSolution (Map.keys deps)))
      Solver.NoOfflineSolution ->
        return (Left (Exit.InitNoOfflineSolution (Map.keys deps)))
      Solver.Ok details ->
        let outline =
              if _isPackage flags
                then pkgOutline
                else appOutlineFromSolverDetails details
         in do
              Dir.createDirectoryIfMissing True "src"
              Outline.write "." outline
              putStrLn "Okay, I created it."
              return (Right ())

pkgOutline :: Outline.Outline
pkgOutline =
  Outline.Pkg $
    Outline.PkgOutline
      Pkg.dummyName
      (Json.fromChars "")
      Licenses.bsd3
      V.one
      (Outline.ExposedList [])
      pkgDefaultDeps
      Map.empty
      Con.defaultGren

appOutlineFromSolverDetails :: (Map.Map Pkg.Name Solver.Details) -> Outline.Outline
appOutlineFromSolverDetails details =
  let solution = Map.map (\(Solver.Details vsn _) -> vsn) details
      directs = Map.intersection solution appDefaultDeps
      indirects = Map.difference solution appDefaultDeps
   in Outline.App $
        Outline.AppOutline
          V.compiler
          (NE.List (Outline.RelativeSrcDir "src") [])
          directs
          indirects
          Map.empty
          Map.empty

appDefaultDeps :: Map.Map Pkg.Name Con.Constraint
appDefaultDeps =
  Map.fromList
    [ (Pkg.core, Con.anything),
      (Pkg.browser, Con.anything)
    ]

pkgDefaultDeps :: Map.Map Pkg.Name Con.Constraint
pkgDefaultDeps =
  Map.fromList
    [ (Pkg.core, Con.untilNextMajor V.one)
    ]
