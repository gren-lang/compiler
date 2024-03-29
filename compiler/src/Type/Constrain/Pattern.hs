{-# OPTIONS_GHC -Wall #-}

module Type.Constrain.Pattern
  ( State (..),
    emptyState,
    add,
  )
where

import AST.Canonical qualified as Can
import Control.Arrow (second)
import Control.Monad (foldM)
import Data.Index qualified as Index
import Data.Map.Strict qualified as Map
import Data.Name qualified as Name
import Gren.ModuleName qualified as ModuleName
import Reporting.Annotation qualified as A
import Reporting.Error.Type qualified as E
import Type.Instantiate qualified as Instantiate
import Type.Type as T

-- ACTUALLY ADD CONSTRAINTS

-- The constraints are stored in reverse order so that adding a new
-- constraint is O(1) and we can reverse it at some later time.
--
data State = State
  { _headers :: Header,
    _vars :: [Variable],
    _revCons :: [Constraint]
  }

type Header = Map.Map Name.Name (A.Located Type)

add :: Can.Pattern -> E.PExpected Type -> State -> IO State
add (A.At region pattern) expectation state =
  case pattern of
    Can.PAnything ->
      return state
    Can.PVar name ->
      return $ addToHeaders region name expectation state
    Can.PAlias realPattern name ->
      add realPattern expectation $
        addToHeaders region name expectation state
    Can.PCtor home typeName (Can.Union typeVars _ _ _) ctorName _ args ->
      addCtor region home typeName typeVars ctorName args expectation state
    Can.PArray patterns ->
      do
        entryVar <- mkFlexVar
        let entryType = VarN entryVar
        let arrayType = AppN ModuleName.array Name.array [entryType]

        (State headers vars revCons) <-
          foldM (addEntry region entryType) state (Index.indexedMap (,) patterns)

        let arrayCon = CPattern region E.PArray arrayType expectation
        return $ State headers (entryVar : vars) (arrayCon : revCons)
    Can.PRecord fieldPatterns ->
      do
        extVar <- mkFlexVar
        let extType = VarN extVar

        let extractNameAndPattern (A.At _ (Can.PRFieldPattern name fieldPattern)) =
              (name, fieldPattern)
        let fields = map extractNameAndPattern fieldPatterns

        fieldVars <- traverse (\(fieldName, _) -> (,) fieldName <$> mkFlexVar) fields
        let fieldTypes = map (fmap VarN) fieldVars
        let recordType = RecordN (Map.fromList fieldTypes) extType
        let recordCon = CPattern region E.PRecord recordType expectation

        (State headers vars revCons) <-
          foldM
            (\s (tipe, fieldPattern) -> simpleAdd fieldPattern tipe s)
            state
            (zip (map snd fieldTypes) (map snd fields))

        return $
          State
            { _headers = headers,
              _vars = map snd fieldVars ++ extVar : vars,
              _revCons = recordCon : revCons
            }
    Can.PInt _ ->
      do
        let (State headers vars revCons) = state
        let intCon = CPattern region E.PInt T.int expectation
        return $ State headers vars (intCon : revCons)
    Can.PStr _ ->
      do
        let (State headers vars revCons) = state
        let strCon = CPattern region E.PStr T.string expectation
        return $ State headers vars (strCon : revCons)
    Can.PChr _ ->
      do
        let (State headers vars revCons) = state
        let chrCon = CPattern region E.PChr T.char expectation
        return $ State headers vars (chrCon : revCons)
    Can.PBool _ _ ->
      do
        let (State headers vars revCons) = state
        let boolCon = CPattern region E.PBool T.bool expectation
        return $ State headers vars (boolCon : revCons)

-- STATE HELPERS

emptyState :: State
emptyState =
  State Map.empty [] []

addToHeaders :: A.Region -> Name.Name -> E.PExpected Type -> State -> State
addToHeaders region name expectation (State headers vars revCons) =
  let tipe = getType expectation
      newHeaders = Map.insert name (A.At region tipe) headers
   in State newHeaders vars revCons

getType :: E.PExpected Type -> Type
getType expectation =
  case expectation of
    E.PNoExpectation tipe -> tipe
    E.PFromContext _ _ tipe -> tipe

simpleAdd :: Can.Pattern -> Type -> State -> IO State
simpleAdd pattern patternType state =
  add pattern (E.PNoExpectation patternType) state

-- CONSTRAIN ARRAY

addEntry :: A.Region -> Type -> State -> (Index.ZeroBased, Can.Pattern) -> IO State
addEntry listRegion tipe state (index, pattern) =
  let expectation =
        E.PFromContext listRegion (E.PArrayEntry index) tipe
   in add pattern expectation state

-- CONSTRAIN CONSTRUCTORS

addCtor :: A.Region -> ModuleName.Canonical -> Name.Name -> [Name.Name] -> Name.Name -> [Can.PatternCtorArg] -> E.PExpected Type -> State -> IO State
addCtor region home typeName typeVarNames ctorName args expectation state =
  do
    varPairs <- traverse (\var -> (,) var <$> nameToFlex var) typeVarNames
    let typePairs = map (second VarN) varPairs
    let freeVarDict = Map.fromList typePairs

    (State headers vars revCons) <-
      foldM (addCtorArg region ctorName freeVarDict) state args

    let ctorType = AppN home typeName (map snd typePairs)
    let ctorCon = CPattern region (E.PCtor ctorName) ctorType expectation

    return $
      State
        { _headers = headers,
          _vars = map snd varPairs ++ vars,
          _revCons = ctorCon : revCons
        }

addCtorArg :: A.Region -> Name.Name -> Map.Map Name.Name Type -> State -> Can.PatternCtorArg -> IO State
addCtorArg region ctorName freeVarDict state (Can.PatternCtorArg index srcType pattern) =
  do
    tipe <- Instantiate.fromSrcType freeVarDict srcType
    let expectation = E.PFromContext region (E.PCtorArg ctorName index) tipe
    add pattern expectation state
