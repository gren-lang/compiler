{-# LANGUAGE OverloadedStrings #-}

module Format
  ( Flags (..),
    run,
    formatByteString,
  )
where

import AbsoluteSrcDir qualified
import Control.Monad (filterM, when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.NonEmptyList qualified as NE
import Directories qualified as Dirs
import File qualified
import Gren.Format qualified as Format
import Gren.Format.Normalize qualified as Normalize
import Gren.Outline qualified as Outline
import Parse.Module qualified as Parse
import Reporting qualified
import Reporting.Doc qualified as D
import Reporting.Exit qualified as Exit
import Reporting.Exit.Help qualified as Help
import Reporting.Task qualified as Task
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.IO qualified

-- FLAGS

data Flags = Flags
  { _skipPrompts :: Bool,
    _stdin :: Bool,
    _validate :: Bool
  }

-- RUN

run :: [FilePath] -> Flags -> IO ()
run paths flags = do
  let action = if _validate flags then validate else format flags
  Reporting.attempt Exit.formatToReport $
    Task.run (action =<< getEnv paths flags)

-- ENV

data Env = Env
  { _inputs :: Inputs
  }

data Inputs
  = Stdin
  | Files [FilePath]
  | Project Parse.ProjectType [FilePath]

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
      do
        (projectType, files) <- sourceDirsFromGrenJson
        resolvedFiles <- resolveFiles files
        return $ Project projectType resolvedFiles
    (False, somePaths) ->
      Files <$> (resolveFiles somePaths)

sourceDirsFromGrenJson :: Task.Task Exit.Format (Parse.ProjectType, [FilePath])
sourceDirsFromGrenJson =
  do
    root <- Task.mio Exit.FormatNoOutline Dirs.findRoot
    outline <- Task.eio Exit.FormatBadOutline $ Outline.read root
    Task.io $
      do
        paths <-
          filterM Dir.doesDirectoryExist
            =<< ( traverse (fmap AbsoluteSrcDir.toFilePath <$> Outline.toAbsoluteSrcDir root) $
                    (NE.toList (Outline.sourceDirs outline))
                )
        return $ case outline of
          Outline.App _ ->
            ( Parse.Application,
              paths
            )
          Outline.Pkg pkgOutline ->
            ( Parse.Package $ Outline._pkg_name pkgOutline,
              paths
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
      else
        if FilePath.takeExtension path == ".gren"
          then return [path]
          else return []
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
        case formatByteString Parse.Application original of
          Nothing ->
            error "TODO: report error"
          Just formatted ->
            Task.io $ B.hPutBuilder System.IO.stdout formatted
    Files paths ->
      formatFilesOnDisk flags Parse.Application paths
    Project projectType paths ->
      formatFilesOnDisk flags projectType paths

validate :: Env -> Task.Task Exit.Format ()
validate (Env inputs) = do
  case inputs of
    Stdin ->
      do
        original <- Task.io BS.getContents
        case formatByteString Parse.Application original of
          Nothing ->
            error "TODO: report error"
          Just formatted ->
            when (BSL.fromStrict original /= B.toLazyByteString formatted) $
              Task.throw Exit.FormatValidateNotCorrectlyFormatted
    Files paths ->
      validateFiles Parse.Application paths
    Project projectType paths ->
      validateFiles projectType paths

validateFiles :: Parse.ProjectType -> [FilePath] -> Task.Task Exit.Format ()
validateFiles projectType paths = do
  validationResults <- mapM (validateFile projectType) paths
  when (any (== False) validationResults) $
    Task.throw Exit.FormatValidateNotCorrectlyFormatted

validateFile :: Parse.ProjectType -> FilePath -> Task.Task Exit.Format Bool
validateFile projectType path =
  assertFileExists path >> Task.io (validateExistingFile projectType path)

validateExistingFile :: Parse.ProjectType -> FilePath -> IO Bool
validateExistingFile projectType path = do
  putStr ("Validating " ++ path)
  original <- File.readUtf8 path
  case formatByteString projectType original of
    Nothing -> do
      -- TODO: report error
      _ <- Help.toStdout (" " <> D.red "ERROR: could not parse file" <> "\n")
      pure False
    Just formatted -> do
      let isFormatted = B.toLazyByteString formatted == BSL.fromStrict original
          status = if isFormatted then D.green "VALID" else D.red "INVALID"
      Help.toStdout (" " <> status <> "\n")
      pure isFormatted

formatFilesOnDisk :: Flags -> Parse.ProjectType -> [FilePath] -> Task.Task Exit.Format ()
formatFilesOnDisk flags projectType paths =
  do
    approved <-
      if not (_skipPrompts flags)
        then Task.io $ Reporting.ask (confirmFormat paths)
        else return True
    if approved
      then mapM_ (formatFile projectType) paths
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

formatFile :: Parse.ProjectType -> FilePath -> Task.Task Exit.Format ()
formatFile projectType path =
  assertFileExists path >> Task.io (formatExistingFile projectType path)

formatExistingFile :: Parse.ProjectType -> FilePath -> IO ()
formatExistingFile projectType path =
  do
    putStr ("Formatting " ++ path)
    original <- File.readUtf8 path
    case formatByteString projectType original of
      Nothing ->
        -- TODO: report error
        Help.toStdout (" " <> D.red "ERROR: could not parse file" <> "\n")
      Just builder ->
        let formatted = B.toLazyByteString builder
         in if formatted == BSL.fromStrict original
              then do
                Help.toStdout (" " <> D.dullwhite "(no changes)" <> "\n")
              else do
                B.writeFile path builder
                Help.toStdout (" " <> D.green "CHANGED" <> "\n")

formatByteString :: Parse.ProjectType -> BS.ByteString -> Maybe B.Builder
formatByteString projectType original =
  case Parse.fromByteString projectType original of
    Left _ ->
      -- TODO: report error
      Nothing
    Right ast ->
      Just (Format.toByteStringBuilder $ Normalize.normalize projectType ast)

assertFileExists :: FilePath -> Task.Task Exit.Format ()
assertFileExists path = do
  exists <- Task.io (Dir.doesFileExist path)
  when (not exists) $
    Task.throw (Exit.FormatPathUnknown path)
