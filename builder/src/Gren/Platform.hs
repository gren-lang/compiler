module Gren.Platform
  ( Platform (..),
    --
    compatible,
    --
    encode,
    decoder,
    fromChars,
    toChars,
  )
where

import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.Utf8 qualified as Utf8
import Json.Decode qualified as D
import Json.Encode qualified as E

data Platform
  = Common
  | Browser
  | Node
  deriving (Eq)

-- COMPATIBILITY

compatible :: Platform -> Platform -> Bool
compatible rootPlatform comparison =
  rootPlatform == comparison || comparison == Common

-- JSON

encode :: Platform -> E.Value
encode platform =
  case platform of
    Common -> E.chars "common"
    Browser -> E.chars "browser"
    Node -> E.chars "node"

decoder :: a -> D.Decoder a Platform
decoder badPlatformError =
  do
    platformStr <- D.string
    case fromChars $ Utf8.toChars platformStr of
      Just platform -> D.succeed platform
      Nothing -> D.failure badPlatformError

fromChars :: [Char] -> Maybe Platform
fromChars value =
  case value of
    "common" -> Just Common
    "browser" -> Just Browser
    "node" -> Just Node
    _ -> Nothing

toChars :: Platform -> [Char]
toChars value =
  case value of
    Common -> "common"
    Browser -> "browser"
    Node -> "node"

-- BINARY

instance Binary Platform where
  put platform =
    case platform of
      Common -> putWord8 0
      Browser -> putWord8 1
      Node -> putWord8 2

  get =
    do
      n <- getWord8
      case n of
        0 -> return Common
        1 -> return Browser
        2 -> return Node
        _ -> fail "binary encoding of Platform was corrupted"
