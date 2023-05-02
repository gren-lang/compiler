{-# LANGUAGE OverloadedStrings #-}

module Generate.SourceMap (SourceMap, generate, sandwhich, toBytes) where

import Data.ByteString.Builder qualified as B
import Generate.JavaScript.Builder qualified as JS

newtype SourceMap = SourceMap B.Builder

generate :: [JS.Mapping] -> SourceMap
generate _ = SourceMap $ B.char7 '\0'

sandwhich :: SourceMap -> B.Builder -> B.Builder
sandwhich (SourceMap mapBytes) sourceBytes =
  sourceBytes
    <> "\n"
    <> "//# sourceMappingURL=data:application/json;base64,"
    <> mapBytes

toBytes :: SourceMap -> B.Builder
toBytes (SourceMap bytes) =
  bytes
