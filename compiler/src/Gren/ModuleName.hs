{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Gren.ModuleName
  ( Raw,
    toChars,
    toFilePath,
    toHyphenPath,
    --
    encode,
    decoder,
    parser,
    --
    Canonical (..),
    basics,
    char,
    string,
    maybe,
    result,
    array,
    dict,
    platform,
    cmd,
    sub,
    task,
    debug,
    virtualDom,
    jsonDecode,
    jsonEncode,
  )
where

import Control.Monad (liftM2)
import Data.Binary (Binary (..))
import Data.Name qualified as Name
import Data.Utf8 qualified as Utf8
import Data.Word (Word8)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Gren.Package qualified as Pkg
import Json.Decode qualified as D
import Json.Encode qualified as E
import Parse.Primitives (Col, Row)
import Parse.Primitives qualified as P
import Parse.Variable qualified as Var
import System.FilePath qualified as FP
import Prelude hiding (maybe)

-- RAW

type Raw = Name.Name

toChars :: Raw -> String
toChars =
  Name.toChars

toFilePath :: Raw -> FilePath
toFilePath name =
  map (\c -> if c == '.' then FP.pathSeparator else c) (Name.toChars name)

toHyphenPath :: Raw -> FilePath
toHyphenPath name =
  map (\c -> if c == '.' then '-' else c) (Name.toChars name)

-- JSON

encode :: Raw -> E.Value
encode =
  E.name

decoder :: D.Decoder (Row, Col) Raw
decoder =
  D.customString parser (,)

-- PARSER

parser :: P.Parser (Row, Col) Raw
parser =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    let (# isGood, newPos, newCol #) = chompStart pos end col
     in if isGood && minusPtr newPos pos < 256
          then
            let !newState = P.State src newPos end indent row newCol
             in cok (Utf8.fromPtr pos newPos) newState
          else
            if col == newCol
              then eerr row newCol (,)
              else cerr row newCol (,)

chompStart :: Ptr Word8 -> Ptr Word8 -> Col -> (# Bool, Ptr Word8, Col #)
chompStart pos end col =
  let !width = Var.getUpperWidth pos end
   in if width == 0
        then (# False, pos, col #)
        else chompInner (plusPtr pos width) end (col + 1)

chompInner :: Ptr Word8 -> Ptr Word8 -> Col -> (# Bool, Ptr Word8, Col #)
chompInner pos end col =
  if pos >= end
    then (# True, pos, col #)
    else
      let !word = P.unsafeIndex pos
          !width = Var.getInnerWidthHelp pos end word
       in if width == 0
            then
              if word == 0x2E {-.-}
                then chompStart (plusPtr pos 1) end (col + 1)
                else (# True, pos, col #)
            else chompInner (plusPtr pos width) end (col + 1)

-- CANONICAL

data Canonical = Canonical
  { _package :: !Pkg.Name,
    _module :: !Name.Name
  }
  deriving (Show)

-- INSTANCES

instance Eq Canonical where
  (==) (Canonical pkg1 name1) (Canonical pkg2 name2) =
    name1 == name2 && pkg1 == pkg2

instance Ord Canonical where
  compare (Canonical pkg1 name1) (Canonical pkg2 name2) =
    case compare name1 name2 of
      LT -> LT
      EQ -> compare pkg1 pkg2
      GT -> GT

instance Binary Canonical where
  put (Canonical a b) = put a >> put b
  get = liftM2 Canonical get get

-- CORE

basics :: Canonical
basics = Canonical Pkg.core Name.basics

char :: Canonical
char = Canonical Pkg.core Name.char

string :: Canonical
string = Canonical Pkg.core Name.string

maybe :: Canonical
maybe = Canonical Pkg.core Name.maybe

result :: Canonical
result = Canonical Pkg.core Name.result

array :: Canonical
array = Canonical Pkg.core Name.array

dict :: Canonical
dict = Canonical Pkg.core Name.dict

platform :: Canonical
platform = Canonical Pkg.core Name.platform

cmd :: Canonical
cmd = Canonical Pkg.core "Platform.Cmd"

sub :: Canonical
sub = Canonical Pkg.core "Platform.Sub"

task :: Canonical
task = Canonical Pkg.core Name.task

debug :: Canonical
debug = Canonical Pkg.core Name.debug

-- HTML

virtualDom :: Canonical
virtualDom = Canonical Pkg.browser Name.virtualDom

-- JSON

jsonDecode :: Canonical
jsonDecode = Canonical Pkg.core "Json.Decode"

jsonEncode :: Canonical
jsonEncode = Canonical Pkg.core "Json.Encode"
