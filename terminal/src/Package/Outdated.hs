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
import Reporting.Exit.Help qualified as RHelp
import Reporting.Task qualified as Task
import Text.PrettyPrint.ANSI.Leijen qualified as P

-- RUN

data Args
  = NoArgs

data Flags = NoFlags

run :: Args -> Flags -> IO ()
run _ _ =
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
  let report = finalizeReport $ Map.foldrWithKey buildReport [] interestingVersions
  if Map.size interestingVersions == 0
    then Task.io $ putStrLn "All dependencies are up to date!"
    else Task.io $ RHelp.toStdout report

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

-- REPORTING

type InProgressReport = [(String, String, String)]

buildReport :: Pkg.Name -> (String, String) -> InProgressReport -> InProgressReport
buildReport pkg (compatible, latest) report =
  ( Pkg.toChars pkg,
    compatible,
    latest
  )
    : report

finalizeReport :: InProgressReport -> P.Doc
finalizeReport report =
  let packageWidth =
        calculateHeaderWidth
          (packageHeader : map (\(pkgNames, _, _) -> pkgNames) report)

      latestCompatibleWidth =
        calculateHeaderWidth
          (latestCompatibleHeader : map (\(_, compatibles, _) -> compatibles) report)

      latestWidth =
        calculateHeaderWidth
          (latestHeader : map (\(_, _, latests) -> latests) report)

      headerRow =
        map
          renderHeader
          [ (packageWidth, packageHeader),
            (latestCompatibleWidth, latestCompatibleHeader),
            (latestWidth, latestHeader)
          ]
   in P.vcat $
        [ P.empty,
          P.hcat headerRow,
          P.vcat $ map (renderLine packageWidth latestCompatibleWidth latestWidth) report,
          P.line
        ]

calculateHeaderWidth :: [String] -> Int
calculateHeaderWidth values =
  3 + (foldr max 0 $ map length values)

renderHeader :: (Int, String) -> P.Doc
renderHeader (width, text) =
  P.fill width $ P.underline $ P.text text

renderLine :: Int -> Int -> Int -> (String, String, String) -> P.Doc
renderLine pkgW lcompW latestW (pkgName, lcomp, latest) =
  P.hcat $
    [ P.fill pkgW $ P.green $ P.text pkgName,
      P.fill lcompW $ P.text lcomp,
      P.fill latestW $ P.text latest
    ]

packageHeader :: String
packageHeader = "Package"

latestCompatibleHeader :: String
latestCompatibleHeader = "Latest Compatible"

latestHeader :: String
latestHeader = "Latest"
