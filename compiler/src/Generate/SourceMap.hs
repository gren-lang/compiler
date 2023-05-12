{-# LANGUAGE OverloadedStrings #-}

module Generate.SourceMap (SourceMap, generate, sandwhich, toBytes) where

import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BLazy
import Data.Function ((&))
import Generate.JavaScript.Builder qualified as JS
import Json.Encode qualified as Json
import Json.String qualified as JStr

newtype SourceMap = SourceMap B.Builder

generate :: [JS.Mapping] -> SourceMap
generate _ =
  Json.object
    [ (JStr.fromChars "version", Json.int 3),
      (JStr.fromChars "sources", Json.array []),
      (JStr.fromChars "sourcesContent", Json.array []),
      (JStr.fromChars "names", Json.array []),
      (JStr.fromChars "mappings", Json.chars "")
    ]
    & Json.encodeUgly
    & B.toLazyByteString
    & BLazy.toStrict
    & Base64.encode
    & B.byteString
    & SourceMap

sandwhich :: SourceMap -> B.Builder -> B.Builder
sandwhich (SourceMap mapBytes) sourceBytes =
  sourceBytes
    <> "\n"
    <> "//# sourceMappingURL=data:application/json;base64,"
    <> mapBytes

toBytes :: SourceMap -> B.Builder
toBytes (SourceMap bytes) =
  bytes
