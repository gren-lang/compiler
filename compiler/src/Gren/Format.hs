module Gren.Format (toByteString) where

import qualified AST.Source as Src
import qualified Data.ByteString as BS

toByteString :: Src.Module -> BS.ByteString
toByteString =
  -- TODO: implement actual formating
  undefined
