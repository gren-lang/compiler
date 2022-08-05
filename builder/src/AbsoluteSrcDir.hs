module AbsoluteSrcDir
  ( AbsoluteSrcDir (..),
    fromFilePath,
    addRelative,
    toFilePath,
  )
where

import System.Directory qualified as Dir
import System.FilePath ((</>))

newtype AbsoluteSrcDir
  = AbsoluteSrcDir FilePath

toFilePath :: AbsoluteSrcDir -> FilePath
toFilePath (AbsoluteSrcDir path) = path

fromFilePath :: FilePath -> IO AbsoluteSrcDir
fromFilePath srcDir =
  AbsoluteSrcDir
    <$> Dir.canonicalizePath srcDir

addRelative :: AbsoluteSrcDir -> FilePath -> FilePath
addRelative (AbsoluteSrcDir srcDir) path =
  srcDir </> path
