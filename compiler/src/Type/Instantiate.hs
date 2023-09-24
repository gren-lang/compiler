{-# OPTIONS_GHC -Wall #-}

module Type.Instantiate
  ( FreeVars,
    fromSrcType,
  )
where

import AST.Canonical qualified as Can
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Name qualified as Name
import Type.Type

-- FREE VARS

type FreeVars =
  Map.Map Name.Name Type

-- FROM SOURCE TYPE

fromSrcType :: Map.Map Name.Name Type -> Can.Type -> IO Type
fromSrcType freeVars sourceType =
  case sourceType of
    Can.TLambda arg result ->
      FunN
        <$> fromSrcType freeVars arg
        <*> fromSrcType freeVars result
    Can.TVar name ->
      return (freeVars ! name)
    Can.TType home name args ->
      AppN home name <$> traverse (fromSrcType freeVars) args
    Can.TAlias home name args aliasedType ->
      do
        targs <- traverse (traverse (fromSrcType freeVars)) args
        AliasN home name targs
          <$> case aliasedType of
            Can.Filled realType ->
              fromSrcType freeVars realType
            Can.Holey realType ->
              fromSrcType (Map.fromList targs) realType
    Can.TRecord fields maybeExt ->
      RecordN
        <$> traverse (fromSrcFieldType freeVars) fields
        <*> case maybeExt of
          Nothing ->
            return EmptyRecordN
          Just ext ->
            return (freeVars ! ext)
    Can.TAliasConstraint home name ->
      return $ AliasConstraint home name

fromSrcFieldType :: Map.Map Name.Name Type -> Can.FieldType -> IO Type
fromSrcFieldType freeVars (Can.FieldType _ tipe) =
  fromSrcType freeVars tipe
