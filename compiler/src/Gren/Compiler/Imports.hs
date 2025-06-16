module Gren.Compiler.Imports
  ( defaults,
  )
where

import AST.Source qualified as Src
import AST.SourceComments qualified as SC
import Data.Name qualified as Name
import Gren.ModuleName qualified as ModuleName
import Reporting.Annotation qualified as A

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
  let maybeAliasWithComments = fmap (,(SC.ImportAliasComments [] [])) maybeAlias
   in Src.Import (A.At A.zero name) maybeAliasWithComments exposing Nothing (SC.ImportComments [] [])

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
