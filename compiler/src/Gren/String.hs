{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Gren.String
  ( String,
    StringFormat (..),
    toChars,
    fromChars,
    toBuilder,
    Chunk (..),
    fromChunks,
  )
where

import Data.Binary (Binary, get, put)
import Data.Bits (shiftR, (.&.))
import Data.ByteString.Builder qualified as B
import Data.Utf8 (MBA, copyFromPtr, freeze, newByteArray, writeWord8)
import Data.Utf8 qualified as Utf8
import GHC.Exts (Ptr, RealWorld)
import GHC.IO (stToIO, unsafeDupablePerformIO)
import GHC.ST (ST)
import GHC.Word (Word8)
import Prelude hiding (String)

-- STRINGS

type String =
  Utf8.Utf8 GREN_STRING

data GREN_STRING

data StringFormat
  = SingleLineString
  | MultilineString
  deriving (Show)

-- HELPERS

toChars :: String -> [Char]
toChars =
  Utf8.toChars

fromChars :: [Char] -> String
fromChars =
  Utf8.fromChars

toBuilder :: String -> B.Builder
toBuilder =
  Utf8.toBuilder

-- FROM CHUNKS

data Chunk
  = Slice (Ptr Word8) Int
  | Escape Word8
  | CodePoint Int

fromChunks :: [Chunk] -> String
fromChunks chunks =
  unsafeDupablePerformIO
    ( stToIO
        ( do
            let !len = sum (map chunkToWidth chunks)
            mba <- newByteArray len
            writeChunks mba 0 chunks
            freeze mba
        )
    )

chunkToWidth :: Chunk -> Int
chunkToWidth chunk =
  case chunk of
    Slice _ len -> len
    Escape _ -> 2
    CodePoint c -> if c < 0xFFFF then 6 else 12

writeChunks :: MBA RealWorld -> Int -> [Chunk] -> ST RealWorld ()
writeChunks mba offset chunks =
  case chunks of
    [] ->
      return ()
    chunk : chunks ->
      case chunk of
        Slice ptr len ->
          do
            copyFromPtr ptr mba offset len
            let !newOffset = offset + len
            writeChunks mba newOffset chunks
        Escape word ->
          do
            writeWord8 mba offset 0x5C {- \ -}
            writeWord8 mba (offset + 1) word
            let !newOffset = offset + 2
            writeChunks mba newOffset chunks
        CodePoint code ->
          if code < 0xFFFF
            then do
              writeCode mba offset code
              let !newOffset = offset + 6
              writeChunks mba newOffset chunks
            else do
              let (hi, lo) = divMod (code - 0x10000) 0x400
              writeCode mba (offset) (hi + 0xD800)
              writeCode mba (offset + 6) (lo + 0xDC00)
              let !newOffset = offset + 12
              writeChunks mba newOffset chunks

writeCode :: MBA RealWorld -> Int -> Int -> ST RealWorld ()
writeCode mba offset code =
  do
    writeWord8 mba offset 0x5C {- \ -}
    writeWord8 mba (offset + 1) 0x75 {- u -}
    writeHex mba (offset + 2) (shiftR code 12)
    writeHex mba (offset + 3) (shiftR code 8)
    writeHex mba (offset + 4) (shiftR code 4)
    writeHex mba (offset + 5) code

writeHex :: MBA RealWorld -> Int -> Int -> ST RealWorld ()
writeHex mba !offset !bits =
  do
    let !n = fromIntegral bits .&. 0x0F
    writeWord8 mba offset (if n < 10 then 0x30 + n else 0x37 + n)

-- BINARY

instance Binary (Utf8.Utf8 GREN_STRING) where
  get = Utf8.getVeryLong
  put = Utf8.putVeryLong
