module Deps.Package
  ( getVersions,
    bumpPossibilities,
    installPackageVersion,
  )
where

import Data.List qualified as List
import Directories qualified as Dirs
import Git qualified
import Gren.Magnitude qualified as M
import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import System.Directory qualified as Dir

-- GET VERSIONS

getVersions :: Dirs.PackageCache -> Pkg.Name -> IO (Either Git.Error (V.Version, [V.Version]))
getVersions cache name = do
  let repoPath = Dirs.basePackage cache name
  repoExists <- Dir.doesDirectoryExist repoPath
  retVal <-
    if repoExists
      then Git.update name repoPath
      else Git.clone (Git.githubUrl name) repoPath
  case retVal of
    Left problem ->
      return $ Left problem
    Right () ->
      Git.tags repoPath

-- GET POSSIBILITIES

bumpPossibilities :: (V.Version, [V.Version]) -> [(V.Version, V.Version, M.Magnitude)]
bumpPossibilities (latest, previous) =
  let allVersions = reverse (latest : previous)
      minorPoints = map last (List.groupBy sameMajor allVersions)
      patchPoints = map last (List.groupBy sameMinor allVersions)
   in (latest, V.bumpMajor latest, M.MAJOR)
        : map (\v -> (v, V.bumpMinor v, M.MINOR)) minorPoints
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
      let basePkgPath = Dirs.basePackage cache pkg
      basePkgExists <- Dir.doesDirectoryExist basePkgPath
      if basePkgExists
        then do
          updateResult <- Git.update pkg basePkgPath
          case updateResult of
            Left updateErr ->
              return $ Left updateErr
            Right () ->
              Git.localClone basePkgPath vsn versionedPkgPath
        else do
          let gitUrl = Git.githubUrl pkg
          baseCloneResult <- Git.clone gitUrl basePkgPath
          case baseCloneResult of
            Left cloneErr ->
              return $ Left cloneErr
            Right () ->
              Git.localClone basePkgPath vsn versionedPkgPath
