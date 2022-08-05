{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Parse.Symbol
  ( operator,
    BadOperator (..),
    binopCharSet,
  )
where

import Data.Char qualified as Char
import Data.IntSet qualified as IntSet
import Data.Name qualified as Name
import Data.Vector qualified as Vector
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import GHC.Word (Word8)
import Parse.Primitives (Col, Parser, Row)
import Parse.Primitives qualified as P

-- OPERATOR

data BadOperator
  = BadDot
  | BadPipe
  | BadArrow
  | BadEquals
  | BadHasType

operator :: (Row -> Col -> x) -> (BadOperator -> Row -> Col -> x) -> Parser x Name.Name
operator toExpectation toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    let !newPos = chompOps pos end
     in if pos == newPos
          then eerr row col toExpectation
          else case Name.fromPtr pos newPos of
            "." -> eerr row col (toError BadDot)
            "|" -> cerr row col (toError BadPipe)
            "->" -> cerr row col (toError BadArrow)
            "=" -> cerr row col (toError BadEquals)
            ":" -> cerr row col (toError BadHasType)
            op ->
              let !newCol = col + fromIntegral (minusPtr newPos pos)
                  !newState = P.State src newPos end indent row newCol
               in cok op newState

chompOps :: Ptr Word8 -> Ptr Word8 -> Ptr Word8
chompOps pos end =
  if pos < end && isBinopCharHelp (P.unsafeIndex pos)
    then chompOps (plusPtr pos 1) end
    else pos

isBinopCharHelp :: Word8 -> Bool
isBinopCharHelp word =
  word < 128 && Vector.unsafeIndex binopCharVector (fromIntegral word)

binopCharVector :: Vector.Vector Bool
binopCharVector =
  Vector.generate 128 (\i -> IntSet.member i binopCharSet)

binopCharSet :: IntSet.IntSet
binopCharSet =
  IntSet.fromList (map Char.ord "+-/*=.<>:&|^?%!")
