{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Optimize.Port
  ( toEncoder,
    toFlagsDecoder,
    toDecoder,
  )
where

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Utils.Type as Type
import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Gren.ModuleName as ModuleName
import qualified Optimize.Names as Names
import Prelude hiding (maybe, null)

-- ENCODE

toEncoder :: Can.Type -> Names.Tracker Opt.Expr
toEncoder tipe =
  case tipe of
    Can.TAlias _ _ args alias ->
      toEncoder (Type.dealias args alias)
    Can.TLambda _ _ ->
      error "toEncoder: function"
    Can.TVar _ ->
      error "toEncoder: type variable"
    Can.TType _ name args ->
      case args of
        []
          | name == Name.float -> encode "float"
          | name == Name.int -> encode "int"
          | name == Name.bool -> encode "bool"
          | name == Name.string -> encode "string"
          | name == Name.unit -> encode "null"
          | name == Name.value -> Names.registerGlobal ModuleName.basics Name.identity
        [arg]
          | name == Name.maybe -> encodeMaybe arg
          | name == Name.array -> encodeArray arg
        _ ->
          error "toEncoder: bad custom type"
    Can.TRecord _ (Just _) ->
      error "toEncoder: bad record"
    Can.TRecord fields Nothing ->
      let encodeField (name, Can.FieldType _ fieldType) =
            do
              encoder <- toEncoder fieldType
              let value = Opt.Call encoder [Opt.Access (Opt.VarLocal Name.dollar) name]
              return $
                Opt.Record $
                  Map.fromList
                    [ (Name.fromChars "key", Opt.Str (Name.toGrenString name)),
                      (Name.fromChars "value", value)
                    ]
       in do
            object <- encode "object"
            keyValuePairs <- traverse encodeField (Map.toList fields)
            Names.registerFieldDict fields $
              Opt.Function [Name.dollar] (Opt.Call object [Opt.Array keyValuePairs])

-- ENCODE HELPERS

encodeMaybe :: Can.Type -> Names.Tracker Opt.Expr
encodeMaybe tipe =
  do
    null <- encode "null"
    encoder <- toEncoder tipe
    destruct <- Names.registerGlobal ModuleName.maybe "destruct"
    return $
      Opt.Function [Name.dollar] $
        Opt.Call destruct [null, encoder, Opt.VarLocal Name.dollar]

encodeArray :: Can.Type -> Names.Tracker Opt.Expr
encodeArray tipe =
  do
    array <- encode "array"
    encoder <- toEncoder tipe
    return $ Opt.Call array [encoder]

-- FLAGS DECODER

toFlagsDecoder :: Can.Type -> Names.Tracker Opt.Expr
toFlagsDecoder tipe =
  toDecoder tipe

-- DECODE

toDecoder :: Can.Type -> Names.Tracker Opt.Expr
toDecoder tipe =
  case tipe of
    Can.TLambda _ _ ->
      error "functions should not be allowed through input ports"
    Can.TVar _ ->
      error "type variables should not be allowed through input ports"
    Can.TAlias _ _ args alias ->
      toDecoder (Type.dealias args alias)
    Can.TType _ name args ->
      case args of
        []
          | name == Name.float -> decode "float"
          | name == Name.int -> decode "int"
          | name == Name.bool -> decode "bool"
          | name == Name.string -> decode "string"
          | name == Name.unit -> decodeUnit
          | name == Name.value -> decode "value"
        [arg]
          | name == Name.maybe -> decodeMaybe arg
          | name == Name.array -> decodeArray arg
        _ ->
          error "toDecoder: bad type"
    Can.TRecord _ (Just _) ->
      error "toDecoder: bad record"
    Can.TRecord fields Nothing ->
      decodeRecord fields

-- DECODE UNIT

decodeUnit :: Names.Tracker Opt.Expr
decodeUnit =
  do
    null <- decode "null"
    unit <- Names.registerGlobal ModuleName.basics Name.unit
    return (Opt.Call null [unit])

-- DECODE MAYBE

decodeMaybe :: Can.Type -> Names.Tracker Opt.Expr
decodeMaybe tipe =
  do
    nothing <- Names.registerGlobal ModuleName.maybe "Nothing"
    just <- Names.registerGlobal ModuleName.maybe "Just"

    oneOf <- decode "oneOf"
    null <- decode "null"
    map_ <- decode "map"

    subDecoder <- toDecoder tipe

    return $
      Opt.Call
        oneOf
        [ Opt.Array
            [ Opt.Call null [nothing],
              Opt.Call map_ [just, subDecoder]
            ]
        ]

-- DECODE ARRAY

decodeArray :: Can.Type -> Names.Tracker Opt.Expr
decodeArray tipe =
  do
    array <- decode "array"
    decoder <- toDecoder tipe
    return $ Opt.Call array [decoder]

-- DECODE RECORDS

decodeRecord :: Map.Map Name.Name Can.FieldType -> Names.Tracker Opt.Expr
decodeRecord fields =
  let toFieldExpr name _ =
        Opt.VarLocal name

      record =
        Opt.Record (Map.mapWithKey toFieldExpr fields)
   in do
        succeed <- decode "succeed"
        foldM fieldAndThen (Opt.Call succeed [record])
          =<< Names.registerFieldDict fields (Map.toList fields)

fieldAndThen :: Opt.Expr -> (Name.Name, Can.FieldType) -> Names.Tracker Opt.Expr
fieldAndThen decoder (key, Can.FieldType _ tipe) =
  do
    andThen <- decode "andThen"
    field <- decode "field"
    typeDecoder <- toDecoder tipe
    return $
      Opt.Call
        andThen
        [ Opt.Function [key] decoder,
          Opt.Call field [Opt.Str (Name.toGrenString key), typeDecoder]
        ]

-- GLOBALS HELPERS

encode :: Name.Name -> Names.Tracker Opt.Expr
encode name =
  Names.registerGlobal ModuleName.jsonEncode name

decode :: Name.Name -> Names.Tracker Opt.Expr
decode name =
  Names.registerGlobal ModuleName.jsonDecode name
