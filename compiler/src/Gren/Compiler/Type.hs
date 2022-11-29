{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Wno-incomplete-uni-patterns #-}

module Gren.Compiler.Type
  ( Type (..),
    RT.Context (..),
    toDoc,
    DebugMetadata (..),
    Alias (..),
    Union (..),
    encode,
    decoder,
    encodeMetadata,
  )
where

import AST.Source qualified as Src
import Data.Name qualified as Name
import Json.Decode qualified as D
import Json.Encode ((==>))
import Json.Encode qualified as E
import Json.String qualified as Json
import Parse.Primitives qualified as P
import Parse.Type qualified as Type
import Reporting.Annotation qualified as A
import Reporting.Doc qualified as D
import Reporting.Render.Type qualified as RT
import Reporting.Render.Type.Localizer qualified as L

-- TYPES

data Type
  = Lambda Type Type
  | Var Name.Name
  | Type Name.Name [Type]
  | Record [(Name.Name, Type)] (Maybe Name.Name)

data DebugMetadata = DebugMetadata
  { _message :: Type,
    _aliases :: [Alias],
    _unions :: [Union]
  }

data Alias = Alias Name.Name [Name.Name] Type

data Union = Union Name.Name [Name.Name] [(Name.Name, [Type])]

-- TO DOC

toDoc :: L.Localizer -> RT.Context -> Type -> D.Doc
toDoc localizer context tipe =
  case tipe of
    Lambda _ _ ->
      let a : b : cs =
            map (toDoc localizer RT.Func) (collectLambdas tipe)
       in RT.lambda context a b cs
    Var name ->
      D.fromName name
    Type name args ->
      RT.apply
        context
        (D.fromName name)
        (map (toDoc localizer RT.App) args)
    Record fields ext ->
      RT.record
        (map (entryToDoc localizer) fields)
        (fmap D.fromName ext)

entryToDoc :: L.Localizer -> (Name.Name, Type) -> (D.Doc, D.Doc)
entryToDoc localizer (field, fieldType) =
  (D.fromName field, toDoc localizer RT.None fieldType)

collectLambdas :: Type -> [Type]
collectLambdas tipe =
  case tipe of
    Lambda arg body ->
      arg : collectLambdas body
    _ ->
      [tipe]

-- JSON for TYPE

encode :: Type -> E.Value
encode tipe =
  E.chars $ D.toLine (toDoc L.empty RT.None tipe)

decoder :: D.Decoder () Type
decoder =
  let parser =
        P.specialize (\_ _ _ -> ()) (fromRawType . fst . fst <$> Type.expression)
   in D.customString parser (\_ _ -> ())

fromRawType :: Src.Type -> Type
fromRawType (A.At _ astType) =
  case astType of
    Src.TLambda t1 t2 ->
      Lambda (fromRawType t1) (fromRawType t2)
    Src.TVar x ->
      Var x
    Src.TType _ name args ->
      Type name (map (fromRawType . snd) args)
    Src.TTypeQual _ _ name args ->
      Type name (map (fromRawType . snd) args)
    Src.TRecord fields ext ->
      let fromField (A.At _ field, tipe, _) = (field, fromRawType tipe)
       in Record
            (map fromField fields)
            (fmap (A.toValue . fst) ext)

-- JSON for PROGRAM

encodeMetadata :: DebugMetadata -> E.Value
encodeMetadata (DebugMetadata msg aliases unions) =
  E.object
    [ "message" ==> encode msg,
      "aliases" ==> E.object (map toTypeAliasField aliases),
      "unions" ==> E.object (map toCustomTypeField unions)
    ]

toTypeAliasField :: Alias -> (Json.String, E.Value)
toTypeAliasField (Alias name args tipe) =
  ( Json.fromName name,
    E.object
      [ "args" ==> E.list E.name args,
        "type" ==> encode tipe
      ]
  )

toCustomTypeField :: Union -> (Json.String, E.Value)
toCustomTypeField (Union name args constructors) =
  ( Json.fromName name,
    E.object
      [ "args" ==> E.list E.name args,
        "tags" ==> E.object (map toVariantObject constructors)
      ]
  )

toVariantObject :: (Name.Name, [Type]) -> (Json.String, E.Value)
toVariantObject (name, args) =
  (Json.fromName name, E.list encode args)
