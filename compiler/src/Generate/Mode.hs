module Generate.Mode
  ( Mode (..),
    ShortFieldNames,
    shortenFieldNames,
  )
where

import AST.Optimized qualified as Opt
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Name qualified as Name
import Generate.JavaScript.Name qualified as JsName

-- MODE

data Mode
  = Dev
  | Prod ShortFieldNames

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
