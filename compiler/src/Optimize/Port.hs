{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Optimize.Port
  ( toEncoder,
    toFlagsDecoder,
    toDecoder,
  )
where

import AST.Canonical qualified as Can
import AST.Optimized qualified as Opt
import AST.Utils.Type qualified as Type
import Control.Monad (foldM)
import Data.Map qualified as Map
import Data.Name qualified as Name
import Gren.ModuleName qualified as ModuleName
import Optimize.Names qualified as Names
import Reporting.Annotation qualified as A
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
          | name == Name.value -> Names.registerGlobal A.zero ModuleName.basics Name.identity
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
              let value = Opt.Call A.zero encoder [Opt.Access (Opt.VarLocal A.zero Name.dollar) A.zero name]
              return $
                Opt.Record A.zero $
                  Map.fromList
                    [ (A.At A.zero (Name.fromChars "key"), Opt.Str A.zero (Name.toGrenString name)),
                      (A.At A.zero (Name.fromChars "value"), value)
                    ]
       in do
            object <- encode "object"
            keyValuePairs <- traverse encodeField (Map.toList fields)
            Names.registerFieldDict fields $
              Opt.Function [Name.dollar] (Opt.Call A.zero object [Opt.Array A.zero keyValuePairs])

-- ENCODE HELPERS

encodeMaybe :: Can.Type -> Names.Tracker Opt.Expr
encodeMaybe tipe =
  do
    null <- encode "null"
    encoder <- toEncoder tipe
    destruct <- Names.registerGlobal A.zero ModuleName.maybe "destruct"
    return $
      Opt.Function [Name.dollar] $
        Opt.Call A.zero destruct [null, encoder, Opt.VarLocal A.zero Name.dollar]

encodeArray :: Can.Type -> Names.Tracker Opt.Expr
encodeArray tipe =
  do
    array <- encode "array"
    encoder <- toEncoder tipe
    return $ Opt.Call A.zero array [encoder]

-- FLAGS DECODER

toFlagsDecoder :: Can.Type -> Names.Tracker Opt.Expr
toFlagsDecoder =
  toDecoder

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
    succeed <- decode "succeed"
    unit <- Names.registerGlobal A.zero ModuleName.basics Name.unit
    return (Opt.Call A.zero succeed [unit])

-- DECODE MAYBE

decodeMaybe :: Can.Type -> Names.Tracker Opt.Expr
decodeMaybe tipe =
  do
    nothing <- Names.registerGlobal A.zero ModuleName.maybe "Nothing"
    just <- Names.registerGlobal A.zero ModuleName.maybe "Just"

    oneOf <- decode "oneOf"
    null <- decode "null"
    map_ <- decode "map"

    subDecoder <- toDecoder tipe

    return $
      (Opt.Call A.zero)
        oneOf
        [ Opt.Array
            A.zero
            [ Opt.Call A.zero null [nothing],
              Opt.Call A.zero map_ [just, subDecoder]
            ]
        ]

-- DECODE ARRAY

decodeArray :: Can.Type -> Names.Tracker Opt.Expr
decodeArray tipe =
  do
    array <- decode "array"
    decoder <- toDecoder tipe
    return $ Opt.Call A.zero array [decoder]

-- DECODE RECORDS

decodeRecord :: Map.Map Name.Name Can.FieldType -> Names.Tracker Opt.Expr
decodeRecord fields =
  let toFieldExpr name _ =
        Opt.VarLocal A.zero name

      record =
        Opt.Record A.zero (Map.mapKeys (A.At A.zero) (Map.mapWithKey toFieldExpr fields))
   in do
        succeed <- decode "succeed"
        foldM fieldAndThen (Opt.Call A.zero succeed [record])
          =<< Names.registerFieldDict fields (Map.toList fields)

fieldAndThen :: Opt.Expr -> (Name.Name, Can.FieldType) -> Names.Tracker Opt.Expr
fieldAndThen decoder (key, Can.FieldType _ tipe) =
  do
    andThen <- decode "andThen"
    field <- decode "field"
    typeDecoder <- toDecoder tipe
    return $
      (Opt.Call A.zero)
        andThen
        [ Opt.Function [key] decoder,
          Opt.Call A.zero field [Opt.Str A.zero (Name.toGrenString key), typeDecoder]
        ]

-- GLOBALS HELPERS

encode :: Name.Name -> Names.Tracker Opt.Expr
encode name =
  Names.registerGlobal A.zero ModuleName.jsonEncode name

decode :: Name.Name -> Names.Tracker Opt.Expr
decode name =
  Names.registerGlobal A.zero ModuleName.jsonDecode name
