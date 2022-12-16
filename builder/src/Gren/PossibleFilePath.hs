module Gren.PossibleFilePath
  ( PossibleFilePath (..),
    mapWith,
    encodeJson,
    other,
  )
where

import Data.Utf8 qualified as Utf8
import Json.Encode qualified as E

data PossibleFilePath a
  = Is FilePath
  | Other a

mapWith :: (a -> b) -> PossibleFilePath a -> PossibleFilePath b
mapWith fn possibleFP =
  case possibleFP of
    Is filePath -> Is filePath
    Other a -> Other $ fn a

other :: PossibleFilePath a -> Maybe a
other possibleFP =
  case possibleFP of
    Is _ -> Nothing
    Other a -> Just a

encodeJson :: (a -> E.Value) -> PossibleFilePath a -> E.Value
encodeJson encoderForNonFP possibleFP =
  case possibleFP of
    Is filePath ->
      E.string $ Utf8.fromChars $ "file:" ++ filePath
    Other a ->
      encoderForNonFP a
