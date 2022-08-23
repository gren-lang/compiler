module Generate.Mode
  ( Mode (..),
    isDebug,
    ShortFieldNames,
    shortenFieldNames,
  )
where

import AST.Optimized qualified as Opt
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Name qualified as Name
import Generate.JavaScript.Name qualified as JsName
import Gren.Compiler.Type.Extract qualified as Extract

-- MODE

data Mode
  = Dev (Maybe Extract.Types)
  | Prod ShortFieldNames

isDebug :: Mode -> Bool
isDebug mode =
  case mode of
    Dev mi -> Maybe.isJust mi
    Prod _ -> False

-- SHORTEN FIELD NAMES

type ShortFieldNames =
  Map.Map Name.Name JsName.Name

shortenFieldNames :: Opt.GlobalGraph -> ShortFieldNames
shortenFieldNames (Opt.GlobalGraph _ frequencies) =
  Map.foldr addToShortNames Map.empty $
    Map.foldrWithKey addToBuckets Map.empty frequencies

addToBuckets :: Name.Name -> Int -> Map.Map Int [Name.Name] -> Map.Map Int [Name.Name]
addToBuckets field frequency buckets =
  Map.insertWith (++) frequency [field] buckets

addToShortNames :: [Name.Name] -> ShortFieldNames -> ShortFieldNames
addToShortNames fields shortNames =
  List.foldl' addField shortNames fields

addField :: ShortFieldNames -> Name.Name -> ShortFieldNames
addField shortNames field =
  let rename = JsName.fromInt (Map.size shortNames)
   in Map.insert field rename shortNames
