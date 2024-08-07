{-# LANGUAGE OverloadedStrings #-}

module Generate.SourceMap (SourceMap, wrap, generateOnto) where

import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BLazy
import Data.Function ((&))
import Data.List as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Ord qualified
import GHC.Word (Word16)
import Generate.JavaScript.Builder qualified as JS
import Generate.JavaScript.Name qualified as JsName
import Generate.VLQ qualified as VLQ
import Gren.ModuleName qualified as ModuleName
import Json.Encode qualified as Json
import Json.String qualified as JStr

newtype SourceMap = SourceMap [JS.Mapping]

wrap :: [JS.Mapping] -> SourceMap
wrap = SourceMap

generateOnto :: Int -> Map.Map ModuleName.Canonical String -> SourceMap -> B.Builder -> B.Builder
generateOnto leadingLines moduleSources (SourceMap mappings) sourceBytes =
  sourceBytes
    <> "\n"
    <> "//# sourceMappingURL=data:application/json;base64,"
    <> generate leadingLines moduleSources mappings

generate :: Int -> Map.Map ModuleName.Canonical String -> [JS.Mapping] -> B.Builder
generate leadingLines moduleSources mappings =
  mappings
    & map (\mapping -> mapping {JS._m_gen_line = JS._m_gen_line mapping + fromIntegral leadingLines})
    & parseMappings
    & mappingsToJson moduleSources
    & Json.encode
    & B.toLazyByteString
    & BLazy.toStrict
    & Base64.encode
    & B.byteString

data Mappings = Mappings
  { _m_sources :: OrderedListBuilder ModuleName.Canonical,
    _m_names :: OrderedListBuilder JsName.Name,
    _m_segment_accounting :: SegmentAccounting,
    _m_vlqs :: B.Builder
  }

data SegmentAccounting = SegmentAccounting
  { _sa_prev_col :: Maybe Word16,
    _sa_prev_source_idx :: Maybe Int,
    _sa_prev_source_line :: Maybe Int,
    _sa_prev_source_col :: Maybe Int,
    _sa_prev_name_idx :: Maybe Int
  }

parseMappings :: [JS.Mapping] -> Mappings
parseMappings mappings =
  let mappingMap = foldr (\mapping acc -> Map.alter (mappingMapUpdater mapping) (JS._m_gen_line mapping) acc) Map.empty mappings
   in parseMappingsHelp 1 (fst $ Map.findMax mappingMap) mappingMap $
        Mappings
          { _m_sources = emptyOrderedListBuilder,
            _m_names = emptyOrderedListBuilder,
            _m_segment_accounting =
              SegmentAccounting
                { _sa_prev_col = Nothing,
                  _sa_prev_source_idx = Nothing,
                  _sa_prev_source_line = Nothing,
                  _sa_prev_source_col = Nothing,
                  _sa_prev_name_idx = Nothing
                },
            _m_vlqs = ""
          }

mappingMapUpdater :: JS.Mapping -> Maybe [JS.Mapping] -> Maybe [JS.Mapping]
mappingMapUpdater toInsert maybeVal =
  case maybeVal of
    Nothing ->
      Just [toInsert]
    Just existing ->
      Just $ toInsert : existing

parseMappingsHelp :: Word16 -> Word16 -> Map Word16 [JS.Mapping] -> Mappings -> Mappings
parseMappingsHelp currentLine lastLine mappingMap acc =
  if currentLine > lastLine
    then acc
    else case Map.lookup currentLine mappingMap of
      Nothing ->
        parseMappingsHelp (currentLine + 1) lastLine mappingMap $
          prepareForNewLine acc
      Just segments ->
        let sortedSegments = List.sortOn (Data.Ord.Down . JS._m_gen_col) segments
         in parseMappingsHelp (currentLine + 1) lastLine mappingMap $
              prepareForNewLine $
                foldr encodeSegment acc sortedSegments

prepareForNewLine :: Mappings -> Mappings
prepareForNewLine (Mappings srcs nms sa vlqs) =
  Mappings
    srcs
    nms
    (sa {_sa_prev_col = Nothing})
    (vlqs <> ";")

encodeSegment :: JS.Mapping -> Mappings -> Mappings
encodeSegment segment (Mappings srcs nms sa vlqs) =
  let newSources = insertIntoOrderedListBuilder (JS._m_src_module segment) srcs
      genCol = JS._m_gen_col segment - 1
      moduleIdx = Maybe.fromMaybe 0 $ lookupIndexOrderedListBuilder (JS._m_src_module segment) newSources
      sourceLine = fromIntegral (JS._m_src_line segment) - 1
      sourceCol = fromIntegral (JS._m_src_col segment) - 1
      genColDelta = fromIntegral genCol - fromIntegral (Maybe.fromMaybe 0 (_sa_prev_col sa))
      moduleIdxDelta = moduleIdx - Maybe.fromMaybe 0 (_sa_prev_source_idx sa)
      sourceLineDelta = sourceLine - fromIntegral (Maybe.fromMaybe 0 (_sa_prev_source_line sa))
      sourceColDelta = sourceCol - fromIntegral (Maybe.fromMaybe 0 (_sa_prev_source_col sa))
      updatedSa =
        SegmentAccounting
          { _sa_prev_col = Just genCol,
            _sa_prev_source_idx = Just moduleIdx,
            _sa_prev_source_line = Just sourceLine,
            _sa_prev_source_col = Just sourceCol,
            _sa_prev_name_idx = _sa_prev_name_idx sa
          }
      vlqPrefix =
        if Maybe.isNothing (_sa_prev_col sa)
          then ""
          else ","
   in case JS._m_src_name segment of
        Just segmentName ->
          let newNames = insertIntoOrderedListBuilder segmentName nms
              nameIdx = Maybe.fromMaybe 0 $ lookupIndexOrderedListBuilder segmentName newNames
              nameIdxDelta = nameIdx - Maybe.fromMaybe 0 (_sa_prev_name_idx sa)
           in Mappings newSources newNames (updatedSa {_sa_prev_name_idx = Just nameIdx}) $
                vlqs
                  <> vlqPrefix
                  <> B.string8 (VLQ.encode genColDelta)
                  <> B.string8 (VLQ.encode moduleIdxDelta)
                  <> B.string8 (VLQ.encode sourceLineDelta)
                  <> B.string8 (VLQ.encode sourceColDelta)
                  <> B.string8 (VLQ.encode nameIdxDelta)
        Nothing ->
          Mappings newSources nms updatedSa $
            vlqs
              <> vlqPrefix
              <> B.string8 (VLQ.encode genColDelta)
              <> B.string8 (VLQ.encode moduleIdxDelta)
              <> B.string8 (VLQ.encode sourceLineDelta)
              <> B.string8 (VLQ.encode sourceColDelta)

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

insertIntoOrderedListBuilder :: (Ord a) => a -> OrderedListBuilder a -> OrderedListBuilder a
insertIntoOrderedListBuilder value builder@(OrderedListBuilder nextIndex values) =
  case Map.lookup value values of
    Just _ ->
      builder
    Nothing ->
      OrderedListBuilder
        { _ab_nextIndex = nextIndex + 1,
          _ab_values = Map.insert value nextIndex values
        }

lookupIndexOrderedListBuilder :: (Ord a) => a -> OrderedListBuilder a -> Maybe Int
lookupIndexOrderedListBuilder value (OrderedListBuilder _ values) =
  Map.lookup value values

orderedListBuilderToList :: OrderedListBuilder a -> [a]
orderedListBuilderToList (OrderedListBuilder _ values) =
  values
    & Map.toList
    & map (\(val, idx) -> (idx, val))
    & Map.fromList
    & Map.elems

mappingsToJson :: Map.Map ModuleName.Canonical String -> Mappings -> Json.Value
mappingsToJson moduleSources (Mappings sources names _sa vlqs) =
  let moduleNames = orderedListBuilderToList sources
   in Json.object
        [ (JStr.fromChars "version", Json.int 3),
          (JStr.fromChars "sources", Json.array $ map (ModuleName.encode . ModuleName._module) moduleNames),
          (JStr.fromChars "sourcesContent", Json.array $ map (\moduleName -> Maybe.maybe Json.null Json.chars $ Map.lookup moduleName moduleSources) moduleNames),
          (JStr.fromChars "names", Json.array $ map (\jsName -> Json.String ("\"" <> JsName.toBuilder jsName <> "\"")) $ orderedListBuilderToList names),
          (JStr.fromChars "mappings", Json.String ("\"" <> vlqs <> "\""))
        ]
