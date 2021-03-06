{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Helpers.Instances where

import qualified AST.Source as Src
import Data.String (IsString (..))
import qualified Data.Utf8 as Utf8
import qualified Reporting.Error.Syntax as E

deriving instance Eq Src.Comment

deriving instance Eq E.Space

instance IsString (Utf8.Utf8 a) where
  fromString = Utf8.fromChars
