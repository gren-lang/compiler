{-# OPTIONS_GHC -Wall #-}

module Directories
  ( details,
    interfaces,
    objects,
    prepublishDir,
    elmi,
    elmo,
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

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.FileLock as Lock
import System.FilePath ((<.>), (</>))
import qualified System.FilePath as FP

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

-- ELMI and ELMO

elmi :: FilePath -> ModuleName.Raw -> FilePath
elmi root name =
  toArtifactPath root name "elmi"

elmo :: FilePath -> ModuleName.Raw -> FilePath
elmo root name =
  toArtifactPath root name "elmo"

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
        exists <- Dir.doesFileExist (FP.joinPath dirs </> "elm.json")
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
