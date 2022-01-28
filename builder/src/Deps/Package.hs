module Deps.Package
  ( getVersions
  )
  where


import qualified System.Directory as Dir

import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Git
import qualified Directories as Dirs



getVersions ::  Dirs.PackageCache -> Pkg.Name -> IO (Either Git.Error (V.Version, [V.Version]))
getVersions cache name = do
    let repoPath = Dirs.basePackage cache name
    repoExists <- Dir.doesDirectoryExist repoPath
    retVal <-
        if repoExists then
          Git.update name repoPath
        else
          Git.clone (Git.githubUrl name) repoPath
    case retVal of
      Left problem ->
        return $ Left problem

      Right () ->
        Git.tags repoPath
