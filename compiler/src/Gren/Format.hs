module Gren.Format (toByteStringBuilder) where

import AST.Source qualified as Src
import Data.ByteString.Builder qualified as B

toByteStringBuilder :: Src.Module -> B.Builder
toByteStringBuilder =
  -- TODO: implement actual formating
  undefined
