module Gren.Format (toByteStringBuilder) where

import qualified AST.Source as Src
import qualified Data.ByteString.Builder as B

toByteStringBuilder :: Src.Module -> B.Builder
toByteStringBuilder =
  -- TODO: implement actual formating
  undefined
