{-# LANGUAGE OverloadedStrings #-}

module Generate.SourceMap (SourceMap, generate, sandwhich, toBytes) where

import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BLazy
import Data.Function ((&))
import Generate.JavaScript.Builder qualified as JS
import Generate.JavaScript.Name qualified as JsName
import Gren.ModuleName qualified as ModuleName
import Json.Encode qualified as Json
import Json.String qualified as JStr

newtype SourceMap = SourceMap B.Builder

generate :: [JS.Mapping] -> SourceMap
generate mappings =
  mappings
    & parseMappings
    & mappingsToJson
    & Json.encode
    & B.toLazyByteString
    & BLazy.toStrict
    & Base64.encode
    & B.byteString
    & SourceMap

data Mappings = Mappings
  { _m_sources :: [String],
    _m_names :: [String],
    _m_vlqs :: [Json.Value]
  }

parseMappings :: [JS.Mapping] -> Mappings
parseMappings mappings =
  parseMappingsHelp mappings $
    Mappings
      { _m_sources = [],
        _m_names = [],
        _m_vlqs = []
      }

parseMappingsHelp :: [JS.Mapping] -> Mappings -> Mappings
parseMappingsHelp mappings acc@(Mappings srcs nms vlqs) =
  case mappings of
    [] -> acc
    first : rest ->
      parseMappingsHelp rest $
        Mappings srcs nms $
          Json.object
            [ (JStr.fromChars "src_line", Json.int $ fromIntegral $ JS._m_src_line first),
              (JStr.fromChars "src_col", Json.int $ fromIntegral $ JS._m_src_col first),
              (JStr.fromChars "src_module", ModuleName.encode $ ModuleName._module $ JS._m_src_module first),
              (JStr.fromChars "src_name", Json.String $ JsName.toBuilder $ JS._m_src_name first),
              (JStr.fromChars "gen_line", Json.int $ fromIntegral $ JS._m_gen_line first),
              (JStr.fromChars "gen_col", Json.int $ fromIntegral $ JS._m_gen_col first)
            ]
            : vlqs

mappingsToJson :: Mappings -> Json.Value
mappingsToJson mappings =
  Json.object
    [ (JStr.fromChars "version", Json.int 3),
      (JStr.fromChars "sources", Json.array $ map Json.chars $ _m_sources mappings),
      (JStr.fromChars "sourcesContent", Json.array []),
      (JStr.fromChars "names", Json.array $ map Json.chars $ _m_names mappings),
      (JStr.fromChars "mappings", Json.array $ _m_vlqs mappings)
    ]

sandwhich :: SourceMap -> B.Builder -> B.Builder
sandwhich (SourceMap mapBytes) sourceBytes =
  sourceBytes
    <> "\n"
    <> "//# sourceMappingURL=data:application/json;base64,"
    <> mapBytes

toBytes :: SourceMap -> B.Builder
toBytes (SourceMap bytes) =
  bytes
