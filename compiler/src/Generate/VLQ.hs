module Generate.VLQ
  ( encode,
  )
where

import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as Bit
import Data.Foldable.WithIndex (ifoldr)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map (Map, (!))
import Data.Map qualified as Map

{- Ported from the Elm package Janiczek/elm-vlq
-}

-- Int is converted to 32-bit representation before encoding
encode :: Int -> String
encode num =
  let numWithSign =
        if num < 0
          then ((negate num .&. usableBits) `Bit.shiftL` 1) .|. 1
          else (num .&. usableBits) `Bit.shiftL` 1
   in encodeHelp numWithSign ""

usableBits :: Int
usableBits =
  0xFFFFFFFF `Bit.shiftR` 1

encodeHelp :: Int -> String -> String
encodeHelp num acc =
  let clamped =
        num .&. 31

      newNum =
        num `Bit.shiftR` 5

      newClamped =
        if newNum > 0
          then clamped .|. 32
          else clamped

      newAcc =
        base64Table ! newClamped : acc
   in if newNum > 0
        then encodeHelp newNum newAcc
        else List.reverse newAcc

base64Table :: Map Int Char
base64Table =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='"
    & ifoldr Map.insert Map.empty
