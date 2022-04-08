{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Gren.Compiler.Imports
  ( defaults,
  )
where

import qualified AST.Source as Src
import qualified Data.Name as Name
import qualified Gren.ModuleName as ModuleName
import qualified Reporting.Annotation as A

-- DEFAULTS

defaults :: [Src.Import]
defaults =
  [ import_ ModuleName.basics Nothing Src.Open,
    import_ ModuleName.debug Nothing closed,
    import_ ModuleName.array Nothing (typeClosed Name.array),
    import_ ModuleName.maybe Nothing (typeOpen Name.maybe),
    import_ ModuleName.result Nothing (typeOpen Name.result),
    import_ ModuleName.string Nothing (typeClosed Name.string),
    import_ ModuleName.char Nothing (typeClosed Name.char),
    import_ ModuleName.platform Nothing (typeClosed Name.program),
    import_ ModuleName.cmd (Just Name.cmd) (typeClosed Name.cmd),
    import_ ModuleName.sub (Just Name.sub) (typeClosed Name.sub)
  ]

import_ :: ModuleName.Canonical -> Maybe Name.Name -> Src.Exposing -> Src.Import
import_ (ModuleName.Canonical _ name) maybeAlias exposing =
  Src.Import (A.At A.zero name) maybeAlias exposing

-- EXPOSING

closed :: Src.Exposing
closed =
  Src.Explicit []

typeOpen :: Name.Name -> Src.Exposing
typeOpen name =
  Src.Explicit [Src.Upper (A.At A.zero name) (Src.Public A.zero)]

typeClosed :: Name.Name -> Src.Exposing
typeClosed name =
  Src.Explicit [Src.Upper (A.At A.zero name) Src.Private]
