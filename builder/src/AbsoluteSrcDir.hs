module AbsoluteSrcDir
  ( AbsoluteSrcDir (..),
    mkAbsoluteSrcDir,
    addRelative,
    toFilePath,
  )
where

import qualified System.Directory as Dir
import System.FilePath ((</>))

newtype AbsoluteSrcDir
  = AbsoluteSrcDir FilePath

toFilePath :: AbsoluteSrcDir -> FilePath
toFilePath (AbsoluteSrcDir path) = path

mkAbsoluteSrcDir :: FilePath -> IO AbsoluteSrcDir
mkAbsoluteSrcDir srcDir =
  AbsoluteSrcDir
    <$> Dir.canonicalizePath srcDir

addRelative :: AbsoluteSrcDir -> FilePath -> FilePath
addRelative (AbsoluteSrcDir srcDir) path =
  srcDir </> path
