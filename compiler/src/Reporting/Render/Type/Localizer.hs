{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Reporting.Render.Type.Localizer
  ( Localizer,
    toDoc,
    toChars,
    empty,
    fromNames,
    fromModule,
  )
where

import AST.Source qualified as Src
import Data.Map qualified as Map
import Data.Name qualified as Name
import Data.Set qualified as Set
import Gren.ModuleName qualified as ModuleName
import Reporting.Annotation qualified as A
import Reporting.Doc qualified as D

-- LOCALIZER

newtype Localizer
  = Localizer (Map.Map Name.Name Import)

data Import = Import
  { _alias :: Maybe Name.Name,
    _exposing :: Exposing
  }

data Exposing
  = All
  | Only (Set.Set Name.Name)

empty :: Localizer
empty =
  Localizer Map.empty

-- LOCALIZE

toDoc :: Localizer -> ModuleName.Canonical -> Name.Name -> D.Doc
toDoc localizer home name =
  D.fromChars (toChars localizer home name)

toChars :: Localizer -> ModuleName.Canonical -> Name.Name -> String
toChars (Localizer localizer) (ModuleName.Canonical _ home) name =
  case Map.lookup home localizer of
    Nothing ->
      Name.toChars home <> "." <> Name.toChars name
    Just (Import alias exposing) ->
      case exposing of
        All ->
          Name.toChars name
        Only set ->
          if Set.member name set
            then Name.toChars name
            else Name.toChars (maybe home id alias) <> "." <> Name.toChars name

-- FROM NAMES

fromNames :: Map.Map Name.Name a -> Localizer
fromNames names =
  Localizer $ Map.map (\_ -> Import Nothing All) names

-- FROM MODULE

fromModule :: Src.Module -> Localizer
fromModule modul@(Src.Module _ _ _ _ imports _ _ _ _ _ _ _) =
  Localizer $
    Map.fromList $
      (Src.getName modul, Import Nothing All) : map (toPair . snd) imports

toPair :: Src.Import -> (Name.Name, Import)
toPair (Src.Import (A.At _ name) _ alias exposing _ _) =
  ( name,
    Import (fmap fst alias) (toExposing exposing)
  )

toExposing :: Src.Exposing -> Exposing
toExposing exposing =
  case exposing of
    Src.Open ->
      All
    Src.Explicit exposedList ->
      Only (foldr addType Set.empty exposedList)

addType :: Src.Exposed -> Set.Set Name.Name -> Set.Set Name.Name
addType exposed types =
  case exposed of
    Src.Lower _ -> types
    Src.Upper (A.At _ name) _ -> Set.insert name types
    Src.Operator _ _ -> types
