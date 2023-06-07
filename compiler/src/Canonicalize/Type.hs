module Canonicalize.Type
  ( toAnnotation,
    canonicalize,
  )
where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Canonicalize.Environment qualified as Env
import Canonicalize.Environment.Dups qualified as Dups
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Name qualified as Name
import Reporting.Annotation qualified as A
import Reporting.Error.Canonicalize qualified as Error
import Reporting.Result qualified as Result

-- RESULT

type Result i w a =
  Result.Result i w Error.Error a

-- TO ANNOTATION

toAnnotation :: Env.Env -> Src.Type -> Result i w Can.Annotation
toAnnotation env srcType =
  do
    tipe <- canonicalize env srcType
    Result.ok $ Can.Forall (addFreeVars Map.empty tipe) tipe

-- CANONICALIZE TYPES

canonicalize :: Env.Env -> Src.Type -> Result i w Can.Type
canonicalize env (A.At typeRegion tipe) =
  case tipe of
    Src.TVar x ->
      Result.ok (Can.TVar x)
    Src.TType region name args ->
      canonicalizeType env typeRegion name (fmap snd args)
        =<< Env.findType region env name
    Src.TTypeQual region home name args ->
      canonicalizeType env typeRegion name (fmap snd args)
        =<< Env.findTypeQual region env home name
    Src.TLambda a b _ ->
      Can.TLambda
        <$> canonicalize env a
        <*> canonicalize env b
    Src.TRecord fields ext ->
      do
        cfields <- sequenceA =<< Dups.checkFields (canonicalizeFields env fields)
        return $ Can.TRecord cfields (fmap (A.toValue . fst) ext)
    Src.TParens inner _ ->
      canonicalize env inner

canonicalizeFields :: Env.Env -> [Src.TRecordField] -> [(A.Located Name.Name, Result i w Can.FieldType, ())]
canonicalizeFields env fields =
  let len = fromIntegral (length fields)
      canonicalizeField index (name, srcType, _) =
        (name, Can.FieldType index <$> canonicalize env srcType, ())
   in zipWith canonicalizeField [0 .. len] fields

-- CANONICALIZE TYPE

canonicalizeType :: Env.Env -> A.Region -> Name.Name -> [Src.Type] -> Env.Type -> Result i w Can.Type
canonicalizeType env region name args info =
  do
    cargs <- traverse (canonicalize env) args
    case info of
      Env.Alias arity home argNames aliasedType ->
        checkArity arity region name args $
          Can.TAlias home name (zip argNames cargs) (Can.Holey aliasedType)
      Env.Union arity home ->
        checkArity arity region name args $
          Can.TType home name cargs

checkArity :: Int -> A.Region -> Name.Name -> [A.Located arg] -> answer -> Result i w answer
checkArity expected region name args answer =
  let actual = length args
   in if expected == actual
        then Result.ok answer
        else Result.throw (Error.BadArity region Error.TypeArity name expected actual)

-- ADD FREE VARS

addFreeVars :: Map.Map Name.Name () -> Can.Type -> Map.Map Name.Name ()
addFreeVars freeVars tipe =
  case tipe of
    Can.TLambda arg result ->
      addFreeVars (addFreeVars freeVars result) arg
    Can.TVar var ->
      Map.insert var () freeVars
    Can.TType _ _ args ->
      List.foldl' addFreeVars freeVars args
    Can.TRecord fields Nothing ->
      Map.foldl addFieldFreeVars freeVars fields
    Can.TRecord fields (Just ext) ->
      Map.foldl addFieldFreeVars (Map.insert ext () freeVars) fields
    Can.TAlias _ _ args _ ->
      List.foldl' (\fvs (_, arg) -> addFreeVars fvs arg) freeVars args

addFieldFreeVars :: Map.Map Name.Name () -> Can.FieldType -> Map.Map Name.Name ()
addFieldFreeVars freeVars (Can.FieldType _ tipe) =
  addFreeVars freeVars tipe
