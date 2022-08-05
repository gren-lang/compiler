module File
  ( Time,
    getTime,
    zeroTime,
    writeBinary,
    readBinary,
    writeUtf8,
    readUtf8,
    writeBuilder,
    exists,
    remove,
    removeDir,
  )
where

import Control.Exception (catch)
import Data.Binary qualified as Binary
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Internal qualified as BS
import Data.Fixed qualified as Fixed
import Data.Time.Clock qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import Foreign.ForeignPtr qualified as FPtr
import GHC.IO.Exception (IOErrorType (InvalidArgument), IOException)
import System.Directory qualified as Dir
import System.FilePath ()
import System.FilePath qualified as FP
import System.IO qualified as IO
import System.IO.Error (annotateIOError, ioeGetErrorType, modifyIOError)

-- TIME

newtype Time = Time Fixed.Pico
  deriving (Eq, Ord)

getTime :: FilePath -> IO Time
getTime path =
  fmap
    (Time . Time.nominalDiffTimeToSeconds . Time.utcTimeToPOSIXSeconds)
    (Dir.getModificationTime path)

zeroTime :: Time
zeroTime =
  Time 0

instance Binary.Binary Time where
  put (Time time) = Binary.put time
  get = Time <$> Binary.get

-- BINARY

writeBinary :: (Binary.Binary a) => FilePath -> a -> IO ()
writeBinary path value =
  do
    let dir = FP.dropFileName path
    Dir.createDirectoryIfMissing True dir
    Binary.encodeFile path value

readBinary :: (Binary.Binary a) => FilePath -> IO (Maybe a)
readBinary path =
  do
    pathExists <- Dir.doesFileExist path
    if pathExists
      then do
        result <- Binary.decodeFileOrFail path
        case result of
          Right a ->
            return (Just a)
          Left (offset, message) ->
            do
              IO.hPutStrLn IO.stderr $
                unlines $
                  [ "+-------------------------------------------------------------------------------",
                    "|  Corrupt File: " ++ path,
                    "|   Byte Offset: " ++ show offset,
                    "|       Message: " ++ message,
                    "|",
                    "| Please report this to https://github.com/gren/compiler/issues",
                    "| Trying to continue anyway.",
                    "+-------------------------------------------------------------------------------"
                  ]
              return Nothing
      else return Nothing

-- WRITE UTF-8

writeUtf8 :: FilePath -> BS.ByteString -> IO ()
writeUtf8 path content =
  withUtf8 path IO.WriteMode $ \handle ->
    BS.hPut handle content

withUtf8 :: FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withUtf8 path mode callback =
  IO.withFile path mode $ \handle ->
    do
      IO.hSetEncoding handle IO.utf8
      callback handle

-- READ UTF-8

readUtf8 :: FilePath -> IO BS.ByteString
readUtf8 path =
  withUtf8 path IO.ReadMode $ \handle ->
    modifyIOError (encodingError path) $
      do
        fileSize <- catch (IO.hFileSize handle) useZeroIfNotRegularFile
        let readSize = max 0 (fromIntegral fileSize) + 1
        hGetContentsSizeHint handle readSize (max 255 readSize)

useZeroIfNotRegularFile :: IOException -> IO Integer
useZeroIfNotRegularFile _ =
  return 0

hGetContentsSizeHint :: IO.Handle -> Int -> Int -> IO BS.ByteString
hGetContentsSizeHint handle =
  readChunks []
  where
    readChunks chunks readSize incrementSize =
      do
        fp <- BS.mallocByteString readSize
        readCount <- FPtr.withForeignPtr fp $ \buf -> IO.hGetBuf handle buf readSize
        let chunk = BS.PS fp 0 readCount
        if readCount < readSize && readSize > 0
          then return $! BS.concat (reverse (chunk : chunks))
          else readChunks (chunk : chunks) incrementSize (min 32752 (readSize + incrementSize))

encodingError :: FilePath -> IOError -> IOError
encodingError path ioErr =
  case ioeGetErrorType ioErr of
    InvalidArgument ->
      annotateIOError
        (userError "Bad encoding; the file must be valid UTF-8")
        ""
        Nothing
        (Just path)
    _ ->
      ioErr

-- WRITE BUILDER

writeBuilder :: FilePath -> B.Builder -> IO ()
writeBuilder path builder =
  IO.withBinaryFile path IO.WriteMode $ \handle ->
    do
      IO.hSetBuffering handle (IO.BlockBuffering Nothing)
      B.hPutBuilder handle builder

-- EXISTS

exists :: FilePath -> IO Bool
exists path =
  Dir.doesFileExist path

-- REMOVE FILES

remove :: FilePath -> IO ()
remove path =
  do
    exists_ <- Dir.doesFileExist path
    if exists_
      then Dir.removeFile path
      else return ()

removeDir :: FilePath -> IO ()
removeDir path =
  do
    exists_ <- Dir.doesDirectoryExist path
    if exists_
      then Dir.removeDirectoryRecursive path
      else return ()
