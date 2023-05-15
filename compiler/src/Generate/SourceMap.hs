{-# LANGUAGE OverloadedStrings #-}

module Generate.SourceMap (SourceMap, wrap, generateOnto) where

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

newtype SourceMap = SourceMap [JS.Mapping]

wrap :: [JS.Mapping] -> SourceMap
wrap mappings = SourceMap mappings

generateOnto :: Int -> SourceMap -> B.Builder -> B.Builder
generateOnto leadingLines (SourceMap mappings) sourceBytes =
  sourceBytes
    <> "\n"
    <> "//# sourceMappingURL=data:application/json;base64,"
    <> generate leadingLines mappings

generate :: Int -> [JS.Mapping] -> B.Builder
generate leadingLines mappings =
  mappings
    & map (\mapping -> mapping {JS._m_gen_line = (JS._m_gen_line mapping) + fromIntegral leadingLines})
    & parseMappings
    & mappingsToJson
    & Json.encode
    & B.toLazyByteString
    & BLazy.toStrict
    & Base64.encode
    & B.byteString

data Mappings = Mappings
  { _m_sources :: OrderedListBuilder Name.Name,
    _m_names :: OrderedListBuilder JsName.Name,
    _m_vlqs :: [Json.Value]
  }

parseMappings :: [JS.Mapping] -> Mappings
parseMappings mappings =
  parseMappingsHelp mappings $
    Mappings
      { _m_sources = emptyOrderedListBuilder,
        _m_names = emptyOrderedListBuilder,
        _m_vlqs = []
      }

parseMappingsHelp :: [JS.Mapping] -> Mappings -> Mappings
parseMappingsHelp mappings acc@(Mappings srcs nms vlqs) =
  case mappings of
    [] -> acc
    first : rest ->
      let newSources = insertIntoOrderedListBuilder (ModuleName._module $ JS._m_src_module first) srcs
          newNames = insertIntoOrderedListBuilder (JS._m_src_name first) nms
       in parseMappingsHelp rest $
            Mappings newSources newNames $
              Json.object
                [ (JStr.fromChars "src_line", Json.int $ fromIntegral $ JS._m_src_line first),
                  (JStr.fromChars "src_col", Json.int $ fromIntegral $ JS._m_src_col first),
                  (JStr.fromChars "src_module", ModuleName.encode $ ModuleName._module $ JS._m_src_module first),
                  (JStr.fromChars "src_module_idx", Json.int $ Maybe.fromMaybe 0 $ lookupIndexOrderedListBuilder (ModuleName._module $ JS._m_src_module first) newSources),
                  (JStr.fromChars "src_name", Json.String $ JsName.toBuilder $ JS._m_src_name first),
                  (JStr.fromChars "src_name_idx", Json.int $ Maybe.fromMaybe 0 $ lookupIndexOrderedListBuilder (JS._m_src_name first) newNames),
                  (JStr.fromChars "gen_line", Json.int $ fromIntegral $ JS._m_gen_line first),
                  (JStr.fromChars "gen_col", Json.int $ fromIntegral $ JS._m_gen_col first)
                ]
                : vlqs

-- Array builder

data OrderedListBuilder a = OrderedListBuilder
  { _ab_nextIndex :: Int,
    _ab_values :: Map.Map a Int
  }

emptyOrderedListBuilder :: OrderedListBuilder a
emptyOrderedListBuilder =
  OrderedListBuilder
    { _ab_nextIndex = 0,
      _ab_values = Map.empty
    }

insertIntoOrderedListBuilder :: Ord a => a -> OrderedListBuilder a -> OrderedListBuilder a
insertIntoOrderedListBuilder value builder@(OrderedListBuilder nextIndex values) =
  case Map.lookup value values of
    Just _ ->
      builder
    Nothing ->
      OrderedListBuilder
        { _ab_nextIndex = nextIndex + 1,
          _ab_values = Map.insert value nextIndex values
        }

lookupIndexOrderedListBuilder :: Ord a => a -> OrderedListBuilder a -> Maybe Int
lookupIndexOrderedListBuilder value (OrderedListBuilder _ values) =
  Map.lookup value values

arrayBuilderToList :: OrderedListBuilder a -> [a]
arrayBuilderToList (OrderedListBuilder _ values) =
  values
    & Map.toList
    & map (\(val, idx) -> (idx, val))
    & Map.fromList
    & Map.elems

mappingsToJson :: Mappings -> Json.Value
mappingsToJson (Mappings sources names vlqs) =
  Json.object
    [ (JStr.fromChars "version", Json.int 3),
      (JStr.fromChars "sources", Json.array $ map ModuleName.encode $ arrayBuilderToList sources),
      (JStr.fromChars "sourcesContent", Json.array []),
      (JStr.fromChars "names", Json.array $ map (Json.String . JsName.toBuilder) $ arrayBuilderToList names),
      (JStr.fromChars "mappings", Json.array vlqs)
    ]
