module Gren.PossibleFilePath
  ( PossibleFilePath (..),
    mapWith,
    encodeJson,
    other,
    is,
    toChars,
  )
where

import Data.Utf8 qualified as Utf8
import Json.Encode qualified as E

data PossibleFilePath a
  = Is FilePath
  | Other a
  deriving (Show, Eq)

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

is :: PossibleFilePath a -> Bool
is possibleFP =
  case possibleFP of
    Is _ -> True
    Other _ -> False

encodeJson :: (a -> E.Value) -> PossibleFilePath a -> E.Value
encodeJson encoderForNonFP possibleFP =
  case possibleFP of
    Is filePath ->
      E.string $ Utf8.fromChars $ "local:" ++ filePath
    Other a ->
      encoderForNonFP a

toChars :: (a -> String) -> PossibleFilePath a -> String
toChars otherToString pfp =
  case pfp of
    Is fp -> fp
    Other a -> otherToString a
