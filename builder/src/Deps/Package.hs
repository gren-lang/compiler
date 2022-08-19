module Deps.Package
  ( getVersions,
    getLatestCompatibleVersion,
    latestCompatibleVersionForPackages,
    bumpPossibilities,
    installPackageVersion,
  )
where

import Data.List qualified as List
import Data.Map qualified as Map
import Directories qualified as Dirs
import Git qualified
import Gren.Constraint qualified as C
import Gren.Magnitude qualified as M
import Gren.Outline qualified as Outline
import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import System.Directory qualified as Dir

-- GET VERSIONS

getVersions :: Pkg.Name -> IO (Either Git.Error (V.Version, [V.Version]))
getVersions name =
  Git.tags (Git.githubUrl name)

-- GET LATEST COMPATIBLE VERSION

getLatestCompatibleVersion :: Dirs.PackageCache -> Pkg.Name -> IO (Either () V.Version)
getLatestCompatibleVersion cache name = do
  versionsResult <- getVersions name
  case versionsResult of
    Right (first, rest) ->
      let versionsHighToLow = List.reverse $ List.sort (first : rest)
       in do
            potentiallyCompatibleVersion <- getCompatibleVersion cache name versionsHighToLow
            case potentiallyCompatibleVersion of
              Nothing ->
                return $ Left ()
              Just v ->
                return $ Right v
    Left _ ->
      return $ Left ()

getCompatibleVersion :: Dirs.PackageCache -> Pkg.Name -> [V.Version] -> IO (Maybe V.Version)
getCompatibleVersion cache name versions =
  case versions of
    [] ->
      return Nothing
    vsn : rest -> do
      potentialInstallationError <- installPackageVersion cache name vsn
      case potentialInstallationError of
        Left _ ->
          getCompatibleVersion cache name rest
        Right () -> do
          let pkgPath = Dirs.package cache name vsn
          potentialOutline <- Outline.read pkgPath
          case potentialOutline of
            Right (Outline.Pkg outline) ->
              if C.goodGren (Outline._pkg_gren_version outline)
                then return $ Just vsn
                else getCompatibleVersion cache name rest
            _ ->
              getCompatibleVersion cache name rest

-- LATEST COMPATIBLE VERSION FOR PACKAGES

latestCompatibleVersionForPackages ::
  Dirs.PackageCache ->
  [Pkg.Name] ->
  IO (Either () (Map.Map Pkg.Name C.Constraint))
latestCompatibleVersionForPackages cache pkgs =
  latestCompatibleVersionForPackagesHelp cache pkgs Map.empty

latestCompatibleVersionForPackagesHelp ::
  Dirs.PackageCache ->
  [Pkg.Name] ->
  Map.Map Pkg.Name C.Constraint ->
  IO (Either () (Map.Map Pkg.Name C.Constraint))
latestCompatibleVersionForPackagesHelp cache pkgs result =
  case pkgs of
    [] -> return $ Right result
    pkg : rest -> do
      possibleVersion <- getLatestCompatibleVersion cache pkg
      case possibleVersion of
        Left _ ->
          return $ Left ()
        Right vsn ->
          let newResult = Map.insert pkg (C.untilNextMajor vsn) result
           in latestCompatibleVersionForPackagesHelp cache rest newResult

-- GET POSSIBILITIES

bumpPossibilities :: (V.Version, [V.Version]) -> [(V.Version, V.Version, M.Magnitude)]
bumpPossibilities (latest, previous) =
  let allVersions = reverse (latest : previous)
      minorPoints = map last (List.groupBy sameMajor allVersions)
      patchPoints = map last (List.groupBy sameMinor allVersions)
   in (latest, V.bumpMajor latest, M.MAJOR) :
      map (\v -> (v, V.bumpMinor v, M.MINOR)) minorPoints
        ++ map (\v -> (v, V.bumpPatch v, M.PATCH)) patchPoints

sameMajor :: V.Version -> V.Version -> Bool
sameMajor (V.Version major1 _ _) (V.Version major2 _ _) =
  major1 == major2

sameMinor :: V.Version -> V.Version -> Bool
sameMinor (V.Version major1 minor1 _) (V.Version major2 minor2 _) =
  major1 == major2 && minor1 == minor2

-- INSTALL PACKAGE VERSION

installPackageVersion :: Dirs.PackageCache -> Pkg.Name -> V.Version -> IO (Either Git.Error ())
installPackageVersion cache pkg vsn = do
  let versionedPkgPath = Dirs.package cache pkg vsn
  versionedPkgExists <- Dir.doesDirectoryExist versionedPkgPath
  if versionedPkgExists
    then return $ Right ()
    else do
      Git.clone (Git.githubUrl pkg) vsn versionedPkgPath
