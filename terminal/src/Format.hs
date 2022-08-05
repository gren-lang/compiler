{-# LANGUAGE OverloadedStrings #-}

module Format
  ( Flags (..),
    run,
  )
where

import AbsoluteSrcDir qualified
import Control.Monad (filterM)
import Data.ByteString qualified as BS
import Data.NonEmptyList qualified as NE
import Directories qualified as Dirs
import File qualified
import Gren.Outline qualified as Outline
import Reporting qualified
import Reporting.Doc qualified as D
import Reporting.Exit qualified as Exit
import Reporting.Exit.Help qualified as Help
import Reporting.Task qualified as Task
import System.Directory qualified as Dir
import System.FilePath ((</>))

-- FLAGS

data Flags = Flags
  { _skipPrompts :: Bool,
    _stdin :: Bool
  }

-- RUN

run :: [FilePath] -> Flags -> IO ()
run paths flags =
  Reporting.attempt Exit.formatToReport $
    Task.run (format flags =<< getEnv paths flags)

-- ENV

data Env = Env
  { _inputs :: Inputs
  }

data Inputs
  = Stdin
  | Files [FilePath]

getEnv :: [FilePath] -> Flags -> Task.Task Exit.Format Env
getEnv paths flags =
  Env <$> (resolveInputPaths paths flags)

resolveInputPaths :: [FilePath] -> Flags -> Task.Task Exit.Format Inputs
resolveInputPaths paths flags =
  case (_stdin flags, paths) of
    (True, []) ->
      return Stdin
    (True, _ : _) ->
      Task.throw Exit.FormatStdinWithFiles
    (False, []) ->
      Files <$> (resolveFiles =<< sourceDirsFromGrenJson)
    (False, somePaths) ->
      Files <$> (resolveFiles somePaths)

sourceDirsFromGrenJson :: Task.Task Exit.Format [FilePath]
sourceDirsFromGrenJson =
  do
    maybeRoot <- Task.io Dirs.findRoot
    case maybeRoot of
      Nothing ->
        Task.throw Exit.FormatNoOutline
      Just root ->
        do
          result <- Task.io $ Outline.read root
          case result of
            Left err ->
              Task.throw $ Exit.FormatBadOutline err
            Right outline ->
              Task.io $
                filterM Dir.doesDirectoryExist
                  =<< ( traverse (fmap AbsoluteSrcDir.toFilePath <$> Outline.toAbsoluteSrcDir root) $
                          (NE.toList (Outline.sourceDirs outline) ++ NE.toList (Outline.testDirs outline))
                      )

resolveFiles :: [FilePath] -> Task.Task Exit.Format [FilePath]
resolveFiles paths =
  concat <$> mapM resolveFile paths

resolveFile :: FilePath -> Task.Task Exit.Format [FilePath]
resolveFile path =
  do
    isDir <- Task.io (Dir.doesDirectoryExist path)
    if isDir
      then resolveFiles =<< Task.io (fmap (path </>) . filter (not . ignore) <$> Dir.listDirectory path)
      else return [path]
  where
    ignore dir =
      dir == ".gren"
        || dir == "node_modules"
        || dir == ".git"

-- FORMAT

format :: Flags -> Env -> Task.Task Exit.Format ()
format flags (Env inputs) =
  case inputs of
    Stdin ->
      do
        original <- Task.io BS.getContents
        let formatted = formatByteString original
        Task.io $ BS.putStr formatted
    Files paths ->
      do
        approved <-
          if not (_skipPrompts flags)
            then Task.io $ Reporting.ask (confirmFormat paths)
            else return True
        if approved
          then mapM_ formatFile paths
          else do
            Task.io $ putStrLn "Okay, I did not change anything!"
            return ()

confirmFormat :: [FilePath] -> D.Doc
confirmFormat paths =
  D.stack
    [ D.reflow "This will overwrite the following files to use Gren's preferred style:",
      D.indent 4 $ D.vcat (fmap D.fromChars paths),
      D.reflow "This cannot be undone! Make sure to back up these files before proceeding.",
      D.reflow
        "Are you sure you want to overwrite these files with formatted versions? [Y/n]: "
    ]

formatFile :: FilePath -> Task.Task Exit.Format ()
formatFile path =
  do
    exists <- Task.io (Dir.doesFileExist path)
    if exists
      then do
        Task.io (formatExistingFile path)
      else Task.throw (Exit.FormatPathUnknown path)

formatExistingFile :: FilePath -> IO ()
formatExistingFile path =
  do
    putStr ("Formatting " ++ path)
    original <- File.readUtf8 path
    let formatted = formatByteString original
    if formatted == original
      then do
        Help.toStdout (" " <> D.dullwhite "(no changes)" <> "\n")
      else do
        File.writeUtf8 path formatted
        Help.toStdout (" " <> D.green "CHANGED" <> "\n")

formatByteString :: BS.ByteString -> BS.ByteString
formatByteString original =
  -- TODO: implement actual formating
  original
