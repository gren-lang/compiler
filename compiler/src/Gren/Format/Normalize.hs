{-# LANGUAGE MagicHash #-}

-- | This module is the "normalization" phase that transforms the raw source AST
-- into an AST ready for rendering back into a text representation.
--
-- This is simply a place to put "formatting" logic that doesn't really make sense
-- as a responsibility for the code that is rendering the AST into text.
module Gren.Format.Normalize (normalize) where

import Gren.Package qualified as Pkg
import AST.Source qualified as Src
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Name (Name)
import Gren.Compiler.Imports qualified as Imports
import Reporting.Annotation qualified as A
import Parse.Module qualified as Parse

normalize :: Parse.ProjectType -> Src.Module -> Src.Module
normalize projectType module_ =
  module_
    { Src._imports = mapMaybe (removeDefaultImports projectType) $ Src._imports module_
    }

removeDefaultImports :: Parse.ProjectType -> Src.Import -> Maybe Src.Import
removeDefaultImports projectType import_@(Src.Import name alias exposing) =
  case Map.lookup (A.toValue name) (defaultImports projectType) of
    Just (Src.Import _ defAlias defExposing) ->
      if alias == defAlias && exposingEq exposing defExposing
        then Nothing
        else Just import_
    Nothing -> Just import_

defaultImports :: Parse.ProjectType -> Map Name Src.Import
defaultImports projectType =
  case projectType of
    Parse.Package pkg | pkg == Pkg.core ->
        Map.empty
    _ ->
        Map.fromList $
            fmap (\import_ -> (Src.getImportName import_, import_)) Imports.defaults

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
