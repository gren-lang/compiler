{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Canonicalize.Environment.Local
  ( add,
  )
where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Canonicalize.Environment qualified as Env
import Canonicalize.Environment.Dups qualified as Dups
import Canonicalize.Type qualified as Type
import Control.Monad (foldM)
import Data.Graph qualified as Graph
import Data.Index qualified as Index
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Name qualified as Name
import Gren.ModuleName qualified as ModuleName
import Reporting.Annotation qualified as A
import Reporting.Error.Canonicalize qualified as Error
import Reporting.Result qualified as Result

-- RESULT

type Result i w a =
  Result.Result i w Error.Error a

type Unions = Map.Map Name.Name Can.Union

type Aliases = Map.Map Name.Name Can.Alias

add :: Src.Module -> Env.Env -> Result i w (Env.Env, Unions, Aliases)
add module_ env =
  addCtors module_ =<< addVars module_ =<< addTypes module_ env

-- ADD VARS

addVars :: Src.Module -> Env.Env -> Result i w Env.Env
addVars module_ (Env.Env home vs ts cs bs qvs qts qcs) =
  do
    topLevelVars <- collectVars module_
    let vs2 = Map.union topLevelVars vs
    -- Use union to overwrite foreign stuff.
    Result.ok $ Env.Env home vs2 ts cs bs qvs qts qcs

collectVars :: Src.Module -> Result i w (Map.Map Name.Name Env.Var)
collectVars (Src.Module _ _ _ _ _ values _ _ _ _ _ effects) =
  let addDecl dict (A.At _ (Src.Value (A.At region name) _ _ _ _)) =
        Dups.insert name region (Env.TopLevel region) dict
   in Dups.detect Error.DuplicateDecl $
        List.foldl' addDecl (toEffectDups effects) (fmap snd values)

toEffectDups :: Src.Effects -> Dups.Dict Env.Var
toEffectDups effects =
  case effects of
    Src.NoEffects ->
      Dups.none
    Src.Ports ports _ ->
      let addPort dict (Src.Port (A.At region name) _) =
            Dups.insert name region (Env.TopLevel region) dict
       in List.foldl' addPort Dups.none (fmap snd ports)
    Src.Manager _ manager _ ->
      case manager of
        Src.Cmd (A.At region _) _ ->
          Dups.one "command" region (Env.TopLevel region)
        Src.Sub (A.At region _) _ ->
          Dups.one "subscription" region (Env.TopLevel region)
        Src.Fx (A.At regionCmd _) (A.At regionSub _) _ ->
          Dups.union
            (Dups.one "command" regionCmd (Env.TopLevel regionCmd))
            (Dups.one "subscription" regionSub (Env.TopLevel regionSub))

-- ADD TYPES

addTypes :: Src.Module -> Env.Env -> Result i w Env.Env
addTypes (Src.Module _ _ _ _ _ _ unions aliases _ _ _ _) (Env.Env home vs ts cs bs qvs qts qcs) =
  let addAliasDups dups (A.At _ (Src.Alias (A.At region name) _ _)) = Dups.insert name region () dups
      addUnionDups dups (A.At _ (Src.Union (A.At region name) _ _ _)) = Dups.insert name region () dups
      typeNameDups =
        List.foldl' addUnionDups (List.foldl' addAliasDups Dups.none (fmap snd aliases)) (fmap snd unions)
   in do
        _ <- Dups.detect Error.DuplicateType typeNameDups
        ts1 <- foldM (addUnion home) ts (fmap snd unions)
        addAliases (fmap snd aliases) (Env.Env home vs ts1 cs bs qvs qts qcs)

addUnion :: ModuleName.Canonical -> Env.Exposed Env.Type -> A.Located Src.Union -> Result i w (Env.Exposed Env.Type)
addUnion home types union@(A.At _ (Src.Union (A.At _ name) _ _ _)) =
  do
    arity <- checkUnionFreeVars union
    let one = Env.Specific home (Env.Union arity home)
    Result.ok $ Map.insert name one types

-- ADD TYPE ALIASES

addAliases :: [A.Located Src.Alias] -> Env.Env -> Result i w Env.Env
addAliases aliases env =
  let nodes = map toNode aliases
      sccs = Graph.stronglyConnComp nodes
   in foldM addAlias env sccs

addAlias :: Env.Env -> Graph.SCC (A.Located Src.Alias) -> Result i w Env.Env
addAlias env@(Env.Env home vs ts cs bs qvs qts qcs) scc =
  case scc of
    Graph.AcyclicSCC alias@(A.At _ (Src.Alias (A.At _ name) _ tipe)) ->
      do
        args <- checkAliasFreeVars alias
        ctype <- Type.canonicalize env tipe
        let one = Env.Specific home (Env.Alias (length args) home args ctype)
        let ts1 = Map.insert name one ts
        Result.ok $ Env.Env home vs ts1 cs bs qvs qts qcs
    Graph.CyclicSCC [] ->
      Result.ok env
    Graph.CyclicSCC (alias@(A.At _ (Src.Alias (A.At region name1) _ tipe)) : others) ->
      do
        args <- checkAliasFreeVars alias
        let toName (A.At _ (Src.Alias (A.At _ name) _ _)) = name
        Result.throw (Error.RecursiveAlias region name1 args tipe (map toName others))

-- DETECT TYPE ALIAS CYCLES

toNode :: A.Located Src.Alias -> (A.Located Src.Alias, Name.Name, [Name.Name])
toNode alias@(A.At _ (Src.Alias (A.At _ name) _ tipe)) =
  (alias, name, getEdges [] tipe)

getEdges :: [Name.Name] -> Src.Type -> [Name.Name]
getEdges edges (A.At _ tipe) =
  case tipe of
    Src.TLambda arg result _ ->
      getEdges (getEdges edges arg) result
    Src.TVar _ ->
      edges
    Src.TType _ name args ->
      List.foldl' getEdges (name : edges) (fmap snd args)
    Src.TTypeQual _ _ _ args ->
      List.foldl' getEdges edges (fmap snd args)
    Src.TRecord fields _ ->
      List.foldl' (\es (_, t, _) -> getEdges es t) edges fields
    Src.TParens inner _ ->
      getEdges edges inner

-- CHECK FREE VARIABLES

checkUnionFreeVars :: A.Located Src.Union -> Result i w Int
checkUnionFreeVars (A.At unionRegion (Src.Union (A.At _ name) args ctors _)) =
  let addArg (_, A.At region arg) dict =
        Dups.insert arg region region dict

      addCtorFreeVars (_, _, tipes, _) freeVars =
        List.foldl' addFreeVars freeVars (fmap snd tipes)
   in do
        boundVars <- Dups.detect (Error.DuplicateUnionArg name) (foldr addArg Dups.none args)
        let freeVars = foldr addCtorFreeVars Map.empty ctors
        case Map.toList (Map.difference freeVars boundVars) of
          [] ->
            Result.ok (length args)
          unbound : unbounds ->
            Result.throw $
              Error.TypeVarsUnboundInUnion unionRegion name (map (A.toValue . snd) args) unbound unbounds

checkAliasFreeVars :: A.Located Src.Alias -> Result i w [Name.Name]
checkAliasFreeVars (A.At aliasRegion (Src.Alias (A.At _ name) args tipe)) =
  let addArg (A.At region arg) dict =
        Dups.insert arg region region dict
   in do
        boundVars <- Dups.detect (Error.DuplicateAliasArg name) (foldr addArg Dups.none args)
        let freeVars = addFreeVars Map.empty tipe
        let overlap = Map.size (Map.intersection boundVars freeVars)
        if Map.size boundVars == overlap && Map.size freeVars == overlap
          then Result.ok (map A.toValue args)
          else
            Result.throw $
              Error.TypeVarsMessedUpInAlias
                aliasRegion
                name
                (map A.toValue args)
                (Map.toList (Map.difference boundVars freeVars))
                (Map.toList (Map.difference freeVars boundVars))

addFreeVars :: Map.Map Name.Name A.Region -> Src.Type -> Map.Map Name.Name A.Region
addFreeVars freeVars (A.At region tipe) =
  case tipe of
    Src.TLambda arg result _ ->
      addFreeVars (addFreeVars freeVars arg) result
    Src.TVar name ->
      Map.insert name region freeVars
    Src.TType _ _ args ->
      List.foldl' addFreeVars freeVars (fmap snd args)
    Src.TTypeQual _ _ _ args ->
      List.foldl' addFreeVars freeVars (fmap snd args)
    Src.TRecord fields maybeExt ->
      let extFreeVars =
            case maybeExt of
              Nothing ->
                freeVars
              Just (A.At extRegion ext, _) ->
                Map.insert ext extRegion freeVars
       in List.foldl' (\fvs (_, t, _) -> addFreeVars fvs t) extFreeVars fields
    Src.TParens inner _ ->
      addFreeVars freeVars inner

-- ADD CTORS

addCtors :: Src.Module -> Env.Env -> Result i w (Env.Env, Unions, Aliases)
addCtors (Src.Module _ _ _ _ _ _ unions aliases _ _ _ _) env@(Env.Env home vs ts cs bs qvs qts qcs) =
  do
    unionInfo <- traverse (canonicalizeUnion env) (fmap snd unions)
    aliasInfo <- traverse (canonicalizeAlias env) (fmap snd aliases)

    ctors <-
      Dups.detect Error.DuplicateCtor $
        Dups.union
          (Dups.unions (map snd unionInfo))
          (Dups.unions (map snd aliasInfo))

    let cs2 = Map.union ctors cs

    Result.ok
      ( Env.Env home vs ts cs2 bs qvs qts qcs,
        Map.fromList (map fst unionInfo),
        Map.fromList (map fst aliasInfo)
      )

type CtorDups = Dups.Dict (Env.Info Env.Ctor)

-- CANONICALIZE ALIAS

canonicalizeAlias :: Env.Env -> A.Located Src.Alias -> Result i w ((Name.Name, Can.Alias), CtorDups)
canonicalizeAlias env (A.At _ (Src.Alias (A.At _ name) args tipe)) =
  do
    let vars = map A.toValue args
    ctipe <- Type.canonicalize env tipe
    Result.ok
      ((name, Can.Alias vars ctipe), Dups.none)

-- CANONICALIZE UNION

canonicalizeUnion :: Env.Env -> A.Located Src.Union -> Result i w ((Name.Name, Can.Union), CtorDups)
canonicalizeUnion env@(Env.Env home _ _ _ _ _ _ _) (A.At _ (Src.Union (A.At _ name) avars ctors _)) =
  do
    cctors <- Index.indexedTraverse (canonicalizeCtor env) ctors
    let vars = map (A.toValue . snd) avars
    let alts = map A.toValue cctors
    let union = Can.Union vars alts (length alts) (toOpts ctors)
    Result.ok
      ( (name, union),
        Dups.unions $ map (toCtor home name union) cctors
      )

canonicalizeCtor :: Env.Env -> Index.ZeroBased -> Src.UnionVariant -> Result i w (A.Located Can.Ctor)
canonicalizeCtor env index (_, A.At region ctor, tipes, _) =
  do
    ctipes <- traverse (Type.canonicalize env) (fmap snd tipes)
    Result.ok $
      A.At region $
        Can.Ctor ctor index (length ctipes) ctipes

toOpts :: [Src.UnionVariant] -> Can.CtorOpts
toOpts ctors =
  case ctors of
    [(_, _, [_], _)] ->
      Can.Unbox
    _ ->
      if all (\(_, _, args, _) -> null args) ctors then Can.Enum else Can.Normal

toCtor :: ModuleName.Canonical -> Name.Name -> Can.Union -> A.Located Can.Ctor -> CtorDups
toCtor home typeName union (A.At region (Can.Ctor name index _ args)) =
  Dups.one name region $
    Env.Specific home $
      Env.Ctor home typeName union index args
