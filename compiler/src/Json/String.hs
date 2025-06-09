{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Json.String
  ( String,
    isEmpty,
    --
    fromPtr,
    fromName,
    fromChars,
    fromSnippet,
    fromSnippetUnescaped,
    fromComment,
    --
    toChars,
    toBuilder,
  )
where

import Data.ByteString.Builder qualified as B
import Data.Coerce qualified as Coerce
import Data.Name qualified as Name
import Data.Utf8 (MBA, copyFromPtr, freeze, newByteArray, writeWord8)
import Data.Utf8 qualified as Utf8
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import GHC.Exts (RealWorld)
import GHC.IO (stToIO, unsafeDupablePerformIO, unsafePerformIO)
import GHC.ST (ST)
import Parse.Primitives qualified as P
import Prelude hiding (String)

-- JSON STRINGS

-- INVARIANT: any Json.String is appropriately escaped already
-- PERF: is this the right representation for Json.String? Maybe ByteString instead?
--
type String =
  Utf8.Utf8 JSON_STRING

data JSON_STRING

isEmpty :: String -> Bool
isEmpty =
  Utf8.isEmpty

-- FROM

fromPtr :: Ptr Word8 -> Ptr Word8 -> String
fromPtr =
  Utf8.fromPtr

fromChars :: [Char] -> String
fromChars =
  Utf8.fromChars

fromSnippet :: P.Snippet -> String
fromSnippet =
  Utf8.fromSnippet

fromName :: Name.Name -> String
fromName =
  Coerce.coerce

-- TO

toChars :: String -> [Char]
toChars =
  Utf8.toChars

toBuilder :: String -> B.Builder
toBuilder =
  Utf8.toBuilder

-- FROM SNIPPET UNESCAPED

fromSnippetUnescaped :: P.Snippet -> String
fromSnippetUnescaped (P.Snippet fptr off len _ _) =
  unsafePerformIO $
    withForeignPtr fptr $ \ptr ->
      let !pos = plusPtr ptr off
          !end = plusPtr pos len
          !str = fromChunks (fromSnippetUnescapedHelp pos end pos [])
       in return str

fromSnippetUnescapedHelp :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> [Chunk] -> [Chunk]
fromSnippetUnescapedHelp pos end start revChunks =
  if pos >= end
    then reverse (addSlice start end revChunks)
    else
      let !word = P.unsafeIndex pos
          !nextPos = plusPtr pos 1
       in case word of
            0x5C {-\-}
              | nextPos < end ->
                  let !nextWord = P.unsafeIndex nextPos
                   in case nextWord of
                        0x74 {-t-} -> addLiteral 0x09 pos end start revChunks
                        0x72 {-r-} -> addLiteral 0x0D pos end start revChunks
                        0x6E {-n-} -> addLiteral 0x0A pos end start revChunks
                        0x22 {-"-} -> addLiteral 0x22 pos end start revChunks
                        0x5C {-\-} -> addLiteral 0x5C pos end start revChunks
                        _ ->
                          let !width = P.getCharWidth word
                              !newPos = plusPtr pos width
                           in fromSnippetUnescapedHelp newPos end start revChunks
            _ ->
              let !width = P.getCharWidth word
                  !newPos = plusPtr pos width
               in fromSnippetUnescapedHelp newPos end start revChunks

addLiteral :: Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> [Chunk] -> [Chunk]
addLiteral literal pos end start revChunks =
  let !pos1 = plusPtr pos 2
   in fromSnippetUnescapedHelp pos1 end pos1 (Literal literal : addSlice start pos revChunks)

-- FROM COMMENT

fromComment :: P.Snippet -> String
fromComment (P.Snippet fptr off len _ _) =
  unsafePerformIO $
    withForeignPtr fptr $ \ptr ->
      let !pos = plusPtr ptr off
          !end = plusPtr pos len
          !str = fromChunks (chompChunks pos end pos [])
       in return str

chompChunks :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> [Chunk] -> [Chunk]
chompChunks pos end start revChunks =
  if pos >= end
    then reverse (addSlice start end revChunks)
    else
      let !word = P.unsafeIndex pos
       in case word of
            0x0A {-\n-} -> chompEscape 0x6E {-n-} pos end start revChunks
            0x22 {-"-} -> chompEscape 0x22 {-"-} pos end start revChunks
            0x5C {-\-} -> chompEscape 0x5C {-\-} pos end start revChunks
            0x0D {-\r-} ->
              let !newPos = plusPtr pos 1
               in chompChunks newPos end newPos (addSlice start pos revChunks)
            _ ->
              let !width = P.getCharWidth word
                  !newPos = plusPtr pos width
               in chompChunks newPos end start revChunks

chompEscape :: Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> [Chunk] -> [Chunk]
chompEscape escape pos end start revChunks =
  let !pos1 = plusPtr pos 1
   in chompChunks pos1 end pos1 (Escape escape : addSlice start pos revChunks)

addSlice :: Ptr Word8 -> Ptr Word8 -> [Chunk] -> [Chunk]
addSlice start end revChunks =
  if start == end
    then revChunks
    else Slice start (minusPtr end start) : revChunks

-- FROM CHUNKS

data Chunk
  = Slice (Ptr Word8) Int
  | Literal Word8
  | Escape Word8

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
    Literal _ -> 1
    Escape _ -> 2

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
        Literal word ->
          do
            writeWord8 mba offset word
            let !newOffset = offset + 1
            writeChunks mba newOffset chunks
        Escape word ->
          do
            writeWord8 mba offset 0x5C {- \ -}
            writeWord8 mba (offset + 1) word
            let !newOffset = offset + 2
            writeChunks mba newOffset chunks
