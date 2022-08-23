{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Helpers.Instances where

import AST.Source qualified as Src
import Data.String (IsString (..))
import Data.Utf8 qualified as Utf8
import Reporting.Error.Syntax qualified as E

deriving instance Eq Src.Comment

deriving instance Eq E.Space

instance IsString (Utf8.Utf8 a) where
  fromString = Utf8.fromChars
