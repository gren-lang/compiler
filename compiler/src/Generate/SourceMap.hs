{-# LANGUAGE OverloadedStrings #-}

module Generate.SourceMap (SourceMap, generate, sandwhich, toBytes) where

import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BLazy
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Name qualified as Name
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
  { _m_sources :: ArrayBuilder Name.Name,
    _m_names :: ArrayBuilder JsName.Name,
    _m_vlqs :: [Json.Value]
  }

parseMappings :: [JS.Mapping] -> Mappings
parseMappings mappings =
  parseMappingsHelp mappings $
    Mappings
      { _m_sources = emptyArrayBuilder,
        _m_names = emptyArrayBuilder,
        _m_vlqs = []
      }

parseMappingsHelp :: [JS.Mapping] -> Mappings -> Mappings
parseMappingsHelp mappings acc@(Mappings srcs nms vlqs) =
  case mappings of
    [] -> acc
    first : rest ->
      let newSources = insertIntoArrayBuilder (ModuleName._module $ JS._m_src_module first) srcs
          newNames = insertIntoArrayBuilder (JS._m_src_name first) nms
       in parseMappingsHelp rest $
            Mappings newSources newNames $
              Json.object
                [ (JStr.fromChars "src_line", Json.int $ fromIntegral $ JS._m_src_line first),
                  (JStr.fromChars "src_col", Json.int $ fromIntegral $ JS._m_src_col first),
                  (JStr.fromChars "src_module", ModuleName.encode $ ModuleName._module $ JS._m_src_module first),
                  (JStr.fromChars "src_module_idx", Json.int $ Maybe.fromMaybe 0 $ lookupIndexArrayBuilder (ModuleName._module $ JS._m_src_module first) newSources),
                  (JStr.fromChars "src_name", Json.String $ JsName.toBuilder $ JS._m_src_name first),
                  (JStr.fromChars "src_name_idx", Json.int $ Maybe.fromMaybe 0 $ lookupIndexArrayBuilder (JS._m_src_name first) newNames),
                  (JStr.fromChars "gen_line", Json.int $ fromIntegral $ JS._m_gen_line first),
                  (JStr.fromChars "gen_col", Json.int $ fromIntegral $ JS._m_gen_col first)
                ]
                : vlqs

-- Array builder

data ArrayBuilder a = ArrayBuilder
  { _ab_nextIndex :: Int,
    _ab_values :: Map.Map a Int
  }

emptyArrayBuilder :: ArrayBuilder a
emptyArrayBuilder =
  ArrayBuilder
    { _ab_nextIndex = 0,
      _ab_values = Map.empty
    }

insertIntoArrayBuilder :: Ord a => a -> ArrayBuilder a -> ArrayBuilder a
insertIntoArrayBuilder value builder@(ArrayBuilder nextIndex values) =
  case Map.lookup value values of
    Just _ ->
      builder
    Nothing ->
      ArrayBuilder
        { _ab_nextIndex = nextIndex + 1,
          _ab_values = Map.insert value nextIndex values
        }

lookupIndexArrayBuilder :: Ord a => a -> ArrayBuilder a -> Maybe Int
lookupIndexArrayBuilder value (ArrayBuilder _ values) =
  Map.lookup value values

arrayBuilderToList :: ArrayBuilder a -> [a]
arrayBuilderToList (ArrayBuilder _ values) =
  values
    & Map.toList
    & map (\(val, idx) -> (idx, val))
    & Map.fromList
    & Map.elems

---

mappingsToJson :: Mappings -> Json.Value
mappingsToJson (Mappings sources names vlqs) =
  Json.object
    [ (JStr.fromChars "version", Json.int 3),
      (JStr.fromChars "sources", Json.array $ map ModuleName.encode $ arrayBuilderToList sources),
      (JStr.fromChars "sourcesContent", Json.array []),
      (JStr.fromChars "names", Json.array $ map (Json.String . JsName.toBuilder) $ arrayBuilderToList names),
      (JStr.fromChars "mappings", Json.array vlqs)
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
