{-# OPTIONS_GHC -Wall #-}

module Directories
  ( details,
    interfaces,
    objects,
    prepublishDir,
    greni,
    greno,
    temp,
    findRoot,
    withRootLock,
    withRegistryLock,
    PackageCache,
    getPackageCache,
    package,
    basePackage,
    getReplCache,
    getGrenHome,
  )
where

import Gren.ModuleName qualified as ModuleName
import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import System.Directory qualified as Dir
import System.Environment qualified as Env
import System.FileLock qualified as Lock
import System.FilePath ((<.>), (</>))
import System.FilePath qualified as FP

-- PATHS

projectCache :: FilePath -> FilePath
projectCache root =
  root </> ".gren" </> compilerVersion

details :: FilePath -> FilePath
details root =
  projectCache root </> "d.dat"

interfaces :: FilePath -> FilePath
interfaces root =
  projectCache root </> "i.dat"

objects :: FilePath -> FilePath
objects root =
  projectCache root </> "o.dat"

prepublishDir :: FilePath -> FilePath
prepublishDir root =
  projectCache root </> "prepublish"

compilerVersion :: FilePath
compilerVersion =
  V.toChars V.compiler

-- GRENI and GRENO

greni :: FilePath -> ModuleName.Raw -> FilePath
greni root name =
  toArtifactPath root name "greni"

greno :: FilePath -> ModuleName.Raw -> FilePath
greno root name =
  toArtifactPath root name "greno"

toArtifactPath :: FilePath -> ModuleName.Raw -> String -> FilePath
toArtifactPath root name ext =
  projectCache root </> ModuleName.toHyphenPath name <.> ext

-- TEMP

temp :: FilePath -> String -> FilePath
temp root ext =
  projectCache root </> "temp" <.> ext

-- ROOT

findRoot :: IO (Maybe FilePath)
findRoot =
  do
    dir <- Dir.getCurrentDirectory
    findRootHelp (FP.splitDirectories dir)

findRootHelp :: [String] -> IO (Maybe FilePath)
findRootHelp dirs =
  case dirs of
    [] ->
      return Nothing
    _ : _ ->
      do
        exists <- Dir.doesFileExist (FP.joinPath dirs </> "gren.json")
        if exists
          then return (Just (FP.joinPath dirs))
          else findRootHelp (init dirs)

-- LOCKS

withRootLock :: FilePath -> IO a -> IO a
withRootLock root work =
  do
    let dir = projectCache root
    Dir.createDirectoryIfMissing True dir
    Lock.withFileLock (dir </> "lock") Lock.Exclusive (\_ -> work)

withRegistryLock :: PackageCache -> IO a -> IO a
withRegistryLock (PackageCache dir) work =
  Lock.withFileLock (dir </> "lock") Lock.Exclusive (\_ -> work)

-- PACKAGE CACHES

newtype PackageCache = PackageCache FilePath

getPackageCache :: IO PackageCache
getPackageCache =
  PackageCache <$> getCacheDir "packages"

package :: PackageCache -> Pkg.Name -> V.Version -> FilePath
package (PackageCache dir) name version =
  dir </> Pkg.toFilePath name </> V.toChars version

basePackage :: PackageCache -> Pkg.Name -> FilePath
basePackage (PackageCache dir) name =
  dir </> Pkg.toFilePath name </> "repo.git"

-- CACHE

getReplCache :: IO FilePath
getReplCache =
  getCacheDir "repl"

getCacheDir :: FilePath -> IO FilePath
getCacheDir projectName =
  do
    home <- getGrenHome
    let root = home </> compilerVersion </> projectName
    Dir.createDirectoryIfMissing True root
    return root

getGrenHome :: IO FilePath
getGrenHome =
  do
    maybeCustomHome <- Env.lookupEnv "GREN_HOME"
    case maybeCustomHome of
      Just customHome -> return customHome
      Nothing -> Dir.getXdgDirectory Dir.XdgCache "gren"
