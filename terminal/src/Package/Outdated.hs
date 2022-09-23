{-# LANGUAGE OverloadedStrings #-}

module Package.Outdated
  ( Args (..),
    Flags (..),
    run,
  )
where

import Data.Map qualified as Map
import Deps.Package qualified as Deps
import Directories qualified as Dirs
import Gren.Constraint qualified as C
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import Reporting qualified
import Reporting.Exit qualified as Exit
import Reporting.Task qualified as Task

-- RUN

data Args
  = NoArgs

data Flags = Flags
  { _skipPrompts :: Bool,
    _unsafe :: Bool
  }

run :: Args -> Flags -> IO ()
run _ (Flags _skipPrompts _unafe) =
  Reporting.attempt Exit.outdatedToReport $
    do
      maybeRoot <- Dirs.findRoot
      case maybeRoot of
        Nothing ->
          return (Left Exit.OutdatedNoOutline)
        Just root ->
          Task.run $
            do
              oldOutline <- Task.eio Exit.OutdatedBadOutline $ Outline.read root
              case oldOutline of
                Outline.App appOutline ->
                  listOutdatedAppDeps appOutline
                Outline.Pkg pkgOutline ->
                  listOutdatedPkgDeps pkgOutline

-- LIST OUTDATED

type Task = Task.Task Exit.Outdated

listOutdatedAppDeps :: Outline.AppOutline -> Task ()
listOutdatedAppDeps appOutline =
  let deps =
        Map.union
          (Outline._app_deps_direct appOutline)
          (Outline._app_deps_indirect appOutline)

      asConstraints = Map.map C.exactly deps
   in listOutdatedDeps asConstraints

listOutdatedPkgDeps :: Outline.PkgOutline -> Task ()
listOutdatedPkgDeps pkgOutline =
  listOutdatedDeps $ Outline._pkg_deps pkgOutline

listOutdatedDeps :: Map.Map Pkg.Name C.Constraint -> Task ()
listOutdatedDeps cons = do
  allHigherVersions <- Map.traverseWithKey higherVersions cons
  let interestingVersions = Map.mapMaybe toDisplayStrings allHigherVersions
  Task.io $ print $ show interestingVersions
  return ()

data AvailableVersions
  = NoAvailableVersion
  | MajorAvailable V.Version
  | MajorAndCompatibleAvailable V.Version V.Version

higherVersions :: Pkg.Name -> C.Constraint -> Task AvailableVersions
higherVersions pkg constraint = do
  (highest, lower) <- Task.eio Exit.OutdatedGitTrouble $ Deps.getVersions pkg
  let loweredConstraint = C.expand constraint V.one
  let expandedConstraint = C.untilNextMajor $ C.lowerBound constraint
  let newer = filter (not . C.satisfies loweredConstraint) (highest : lower)
  let newestCompatible = filter (C.satisfies expandedConstraint) newer
  return $ case (newer, newestCompatible) of
    ([], _) -> NoAvailableVersion
    (newest : _, []) -> MajorAvailable newest
    (newest : _, compatible : _) -> MajorAndCompatibleAvailable newest compatible

toDisplayStrings :: AvailableVersions -> Maybe (String, String)
toDisplayStrings vsns =
  case vsns of
    NoAvailableVersion ->
      Nothing
    MajorAvailable major ->
      Just ("up to date", V.toChars major)
    MajorAndCompatibleAvailable major compatible ->
      Just (V.toChars compatible, V.toChars major)
