{-# LANGUAGE MagicHash #-}

-- | This module is the "normalization" phase that transforms the raw source AST
-- into an AST ready for rendering back into a text representation.
--
-- This is simply a place to put "formatting" logic that doesn't really make sense
-- as a responsibility for the code that is rendering the AST into text.
module Gren.Format.Normalize (normalize) where

import AST.Source qualified as Src
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Name (Name)
import Gren.Compiler.Imports qualified
import Reporting.Annotation qualified as A

normalize :: Src.Module -> Src.Module
normalize module_ =
  module_
    { Src._imports = List.sortOn importSortKey $ mapMaybe removeDefaultImports $ Src._imports module_
    }

importSortKey :: Src.Import -> Name
importSortKey (Src.Import name _ _) =
  A.toValue name

removeDefaultImports :: Src.Import -> Maybe Src.Import
removeDefaultImports import_@(Src.Import name alias exposing) =
  case Map.lookup (A.toValue name) defaultImports of
    Just (Src.Import _ defAlias defExposing) ->
      if alias == defAlias && exposingEq exposing defExposing
        then Nothing
        else Just import_
    Nothing -> Just import_

defaultImports :: Map Name Src.Import
defaultImports =
  Map.fromList $ fmap (\import_ -> (Src.getImportName import_, import_)) Gren.Compiler.Imports.defaults

exposingEq :: Src.Exposing -> Src.Exposing -> Bool
exposingEq Src.Open Src.Open = True
exposingEq (Src.Explicit a) (Src.Explicit b) =
  fmap stripRegionsExposed a == fmap stripRegionsExposed b
exposingEq _ _ = False

data SimpleExposed
  = Lower Name
  | Upper Name SimplePrivacy
  | Operator Name
  deriving (Eq)

data SimplePrivacy
  = Public
  | Private
  deriving (Eq)

stripRegionsExposed :: Src.Exposed -> SimpleExposed
stripRegionsExposed exposed =
  case exposed of
    Src.Lower (A.At _ name) -> Lower name
    Src.Upper (A.At _ name) priv -> Upper name (stripRegionsPrivacy priv)
    Src.Operator _ name -> Operator name

stripRegionsPrivacy :: Src.Privacy -> SimplePrivacy
stripRegionsPrivacy priv =
  case priv of
    Src.Public _ -> Public
    Src.Private -> Private
