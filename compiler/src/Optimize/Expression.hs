{-# OPTIONS_GHC -Wall #-}

module Optimize.Expression
  ( optimize,
    destructArgs,
    optimizePotentialTailCall,
  )
where

import AST.Canonical qualified as Can
import AST.Optimized qualified as Opt
import Control.Monad (foldM)
import Data.Index qualified as Index
import Data.Map qualified as Map
import Data.Name qualified as Name
import Data.Set qualified as Set
import Gren.ModuleName qualified as ModuleName
import Optimize.Case qualified as Case
import Optimize.Names qualified as Names
import Reporting.Annotation qualified as A
import Prelude hiding (cycle)

-- OPTIMIZE

type Cycle =
  Set.Set Name.Name

optimize :: Cycle -> Can.Expr -> Names.Tracker Opt.Expr
optimize cycle (A.At region expression) =
  case expression of
    Can.VarLocal name ->
      pure (Opt.VarLocal region name)
    Can.VarTopLevel home name ->
      if Set.member name cycle
        then pure (Opt.VarCycle region home name)
        else Names.registerGlobal region home name
    Can.VarKernel home name ->
      Names.registerKernel home (Opt.VarKernel region home name)
    Can.VarForeign home name _ ->
      Names.registerGlobal region home name
    Can.VarCtor opts home name index _ ->
      Names.registerCtor region home (A.At region name) index opts
    Can.VarDebug home name _ ->
      Names.registerDebug name home region
    Can.VarOperator _ home name _ ->
      Names.registerGlobal region home name
    Can.Chr chr ->
      Names.registerKernel Name.utils (Opt.Chr region chr)
    Can.Str str ->
      pure (Opt.Str region str)
    Can.Int int ->
      pure (Opt.Int region int)
    Can.Float float ->
      pure (Opt.Float region float)
    Can.Array entries ->
      Names.registerKernel Name.array (Opt.Array region)
        <*> traverse (optimize cycle) entries
    Can.Negate expr ->
      do
        func <- Names.registerGlobal region ModuleName.basics Name.negate
        arg <- optimize cycle expr
        pure $ Opt.Call region func [arg]
    Can.Binop _ home name _ left right ->
      do
        optFunc <- Names.registerGlobal region home name
        optLeft <- optimize cycle left
        optRight <- optimize cycle right
        return (Opt.Call region optFunc [optLeft, optRight])
    Can.Lambda args body ->
      do
        (argNames, destructors) <- destructArgs args
        obody <- optimize cycle body
        pure $ Opt.Function argNames (foldr Opt.Destruct obody destructors)
    Can.Call func args ->
      Opt.Call region
        <$> optimize cycle func
        <*> traverse (optimize cycle) args
    Can.If branches finally ->
      let optimizeBranch (condition, branch) =
            (,)
              <$> optimize cycle condition
              <*> optimize cycle branch
       in Opt.If
            <$> traverse optimizeBranch branches
            <*> optimize cycle finally
    Can.Let def body ->
      optimizeDef cycle def =<< optimize cycle body
    Can.LetRec defs body ->
      case defs of
        [def] ->
          Opt.Let
            <$> optimizePotentialTailCallDef cycle def
            <*> optimize cycle body
        _ ->
          do
            obody <- optimize cycle body
            foldM (\bod def -> optimizeDef cycle def bod) obody defs
    Can.LetDestruct pattern expr body ->
      do
        (name, destructs) <- destruct pattern
        oexpr <- optimize cycle expr
        obody <- optimize cycle body
        pure $
          Opt.Let (Opt.Def name oexpr) (foldr Opt.Destruct obody destructs)
    Can.Case expr branches ->
      let optimizeBranch root (Can.CaseBranch pattern branch) =
            do
              destructors <- destructCase root pattern
              obranch <- optimize cycle branch
              pure (pattern, foldr Opt.Destruct obranch destructors)
       in do
            temp <- Names.generate
            oexpr <- optimize cycle expr
            case oexpr of
              Opt.VarLocal _region root ->
                Case.optimize temp root <$> traverse (optimizeBranch root) branches
              _ ->
                do
                  obranches <- traverse (optimizeBranch temp) branches
                  return $ Opt.Let (Opt.Def temp oexpr) (Case.optimize temp temp obranches)
    Can.Accessor field ->
      Names.registerField field (Opt.Accessor field)
    Can.Access record (A.At fieldPosition field) ->
      do
        optRecord <- optimize cycle record
        Names.registerField field (Opt.Access optRecord fieldPosition field)
    Can.Update record updates ->
      Names.registerFieldDict (Map.mapKeys A.toValue updates) (Opt.Update region)
        <*> optimize cycle record
        <*> traverse (optimizeUpdate cycle) updates
    Can.Record fields ->
      Names.registerFieldDict (Map.mapKeys A.toValue fields) (Opt.Record region)
        <*> traverse (optimize cycle) fields

-- UPDATE

optimizeUpdate :: Cycle -> Can.FieldUpdate -> Names.Tracker Opt.Expr
optimizeUpdate cycle (Can.FieldUpdate _ expr) =
  optimize cycle expr

-- DEFINITION

optimizeDef :: Cycle -> Can.Def -> Opt.Expr -> Names.Tracker Opt.Expr
optimizeDef cycle def body =
  case def of
    Can.Def (A.At _ name) args expr ->
      optimizeDefHelp cycle name args expr body
    Can.TypedDef (A.At _ name) _ typedArgs expr _ ->
      optimizeDefHelp cycle name (map fst typedArgs) expr body

optimizeDefHelp :: Cycle -> Name.Name -> [Can.Pattern] -> Can.Expr -> Opt.Expr -> Names.Tracker Opt.Expr
optimizeDefHelp cycle name args expr body =
  do
    oexpr <- optimize cycle expr
    case args of
      [] ->
        pure $ Opt.Let (Opt.Def name oexpr) body
      _ ->
        do
          (argNames, destructors) <- destructArgs args
          let ofunc = Opt.Function argNames (foldr Opt.Destruct oexpr destructors)
          pure $ Opt.Let (Opt.Def name ofunc) body

-- DESTRUCTURING

destructArgs :: [Can.Pattern] -> Names.Tracker ([Name.Name], [Opt.Destructor])
destructArgs args =
  do
    (argNames, destructorLists) <- unzip <$> traverse destruct args
    return (argNames, concat destructorLists)

destructCase :: Name.Name -> Can.Pattern -> Names.Tracker [Opt.Destructor]
destructCase rootName pattern =
  reverse <$> destructHelp (Opt.Root rootName) pattern []

destruct :: Can.Pattern -> Names.Tracker (Name.Name, [Opt.Destructor])
destruct pattern@(A.At _ ptrn) =
  case ptrn of
    Can.PVar name ->
      pure (name, [])
    Can.PAlias subPattern name ->
      do
        revDs <- destructHelp (Opt.Root name) subPattern []
        pure (name, reverse revDs)
    _ ->
      do
        name <- Names.generate
        revDs <- destructHelp (Opt.Root name) pattern []
        pure (name, reverse revDs)

destructHelp :: Opt.Path -> Can.Pattern -> [Opt.Destructor] -> Names.Tracker [Opt.Destructor]
destructHelp path (A.At _ pattern) revDs =
  case pattern of
    Can.PAnything ->
      pure revDs
    Can.PVar name ->
      pure (Opt.Destructor name path : revDs)
    Can.PAlias subPattern name ->
      destructHelp (Opt.Root name) subPattern $
        Opt.Destructor name path : revDs
    Can.PRecord [] ->
      pure revDs
    Can.PRecord [A.At _ (Can.PRFieldPattern name fieldPattern)] ->
      destructHelp (Opt.Field name path) fieldPattern revDs
    Can.PRecord fieldPatterns ->
      case path of
        Opt.Root _ ->
          foldM (destructRecordField path) revDs fieldPatterns
        _ ->
          do
            name <- Names.generate
            foldM
              (destructRecordField (Opt.Root name))
              (Opt.Destructor name path : revDs)
              fieldPatterns
    Can.PArray [] ->
      pure revDs
    Can.PArray [element] ->
      destructHelp (Opt.ArrayIndex Index.first path) element revDs
    Can.PArray elements ->
      let indexedElements = Index.indexedMap (,) elements
       in case path of
            Opt.Root _ ->
              foldM (destructArrayElement path) revDs indexedElements
            _ ->
              do
                name <- Names.generate
                foldM
                  (destructArrayElement (Opt.Root name))
                  (Opt.Destructor name path : revDs)
                  indexedElements
    Can.PChr _ ->
      pure revDs
    Can.PStr _ ->
      pure revDs
    Can.PInt _ ->
      pure revDs
    Can.PBool _ _ ->
      pure revDs
    Can.PCtor _ _ (Can.Union _ _ _ opts) _ _ args ->
      case args of
        [Can.PatternCtorArg _ _ arg] ->
          case opts of
            Can.Normal -> destructHelp (Opt.Index Index.first path) arg revDs
            Can.Unbox -> destructHelp (Opt.Unbox path) arg revDs
            Can.Enum -> destructHelp (Opt.Index Index.first path) arg revDs
        _ ->
          case path of
            Opt.Root _ ->
              foldM (destructCtorArg path) revDs args
            _ ->
              do
                name <- Names.generate
                foldM
                  (destructCtorArg (Opt.Root name))
                  (Opt.Destructor name path : revDs)
                  args

destructRecordField :: Opt.Path -> [Opt.Destructor] -> Can.PatternRecordField -> Names.Tracker [Opt.Destructor]
destructRecordField path revDs (A.At _ (Can.PRFieldPattern name pattern)) =
  destructHelp (Opt.Field name path) pattern revDs

destructArrayElement :: Opt.Path -> [Opt.Destructor] -> (Index.ZeroBased, Can.Pattern) -> Names.Tracker [Opt.Destructor]
destructArrayElement path revDs (index, arg) =
  destructHelp (Opt.ArrayIndex index path) arg revDs

destructCtorArg :: Opt.Path -> [Opt.Destructor] -> Can.PatternCtorArg -> Names.Tracker [Opt.Destructor]
destructCtorArg path revDs (Can.PatternCtorArg index _ arg) =
  destructHelp (Opt.Index index path) arg revDs

-- TAIL CALL

optimizePotentialTailCallDef :: Cycle -> Can.Def -> Names.Tracker Opt.Def
optimizePotentialTailCallDef cycle def =
  case def of
    Can.Def (A.At _ name) args expr ->
      optimizePotentialTailCall cycle name args expr
    Can.TypedDef (A.At _ name) _ typedArgs expr _ ->
      optimizePotentialTailCall cycle name (map fst typedArgs) expr

optimizePotentialTailCall :: Cycle -> Name.Name -> [Can.Pattern] -> Can.Expr -> Names.Tracker Opt.Def
optimizePotentialTailCall cycle name args expr =
  do
    (argNames, destructors) <- destructArgs args
    toTailDef name argNames destructors
      <$> optimizeTail cycle name argNames expr

optimizeTail :: Cycle -> Name.Name -> [Name.Name] -> Can.Expr -> Names.Tracker Opt.Expr
optimizeTail cycle rootName argNames locExpr@(A.At region expression) =
  case expression of
    Can.Call func args ->
      do
        oargs <- traverse (optimize cycle) args

        let isMatchingName =
              case A.toValue func of
                Can.VarLocal name -> rootName == name
                Can.VarTopLevel _ name -> rootName == name
                _ -> False

        if isMatchingName
          then case Index.indexedZipWith (\_ a b -> (a, b)) argNames oargs of
            Index.LengthMatch pairs ->
              pure $ Opt.TailCall rootName pairs
            Index.LengthMismatch _ _ ->
              do
                ofunc <- optimize cycle func
                pure $ Opt.Call region ofunc oargs
          else do
            ofunc <- optimize cycle func
            pure $ Opt.Call region ofunc oargs
    Can.If branches finally ->
      let optimizeBranch (condition, branch) =
            (,)
              <$> optimize cycle condition
              <*> optimizeTail cycle rootName argNames branch
       in Opt.If
            <$> traverse optimizeBranch branches
            <*> optimizeTail cycle rootName argNames finally
    Can.Let def body ->
      optimizeDef cycle def =<< optimizeTail cycle rootName argNames body
    Can.LetRec defs body ->
      case defs of
        [def] ->
          Opt.Let
            <$> optimizePotentialTailCallDef cycle def
            <*> optimizeTail cycle rootName argNames body
        _ ->
          do
            obody <- optimizeTail cycle rootName argNames body
            foldM (\bod def -> optimizeDef cycle def bod) obody defs
    Can.LetDestruct pattern expr body ->
      do
        (dname, destructors) <- destruct pattern
        oexpr <- optimize cycle expr
        obody <- optimizeTail cycle rootName argNames body
        pure $
          Opt.Let (Opt.Def dname oexpr) (foldr Opt.Destruct obody destructors)
    Can.Case expr branches ->
      let optimizeBranch root (Can.CaseBranch pattern branch) =
            do
              destructors <- destructCase root pattern
              obranch <- optimizeTail cycle rootName argNames branch
              pure (pattern, foldr Opt.Destruct obranch destructors)
       in do
            temp <- Names.generate
            oexpr <- optimize cycle expr
            case oexpr of
              Opt.VarLocal _region root ->
                Case.optimize temp root <$> traverse (optimizeBranch root) branches
              _ ->
                do
                  obranches <- traverse (optimizeBranch temp) branches
                  return $ Opt.Let (Opt.Def temp oexpr) (Case.optimize temp temp obranches)
    _ ->
      optimize cycle locExpr

-- DETECT TAIL CALLS

toTailDef :: Name.Name -> [Name.Name] -> [Opt.Destructor] -> Opt.Expr -> Opt.Def
toTailDef name argNames destructors body =
  if hasTailCall body
    then Opt.TailDef name argNames (foldr Opt.Destruct body destructors)
    else Opt.Def name (Opt.Function argNames (foldr Opt.Destruct body destructors))

hasTailCall :: Opt.Expr -> Bool
hasTailCall expression =
  case expression of
    Opt.TailCall _ _ ->
      True
    Opt.If branches finally ->
      hasTailCall finally || any (hasTailCall . snd) branches
    Opt.Let _ body ->
      hasTailCall body
    Opt.Destruct _ body ->
      hasTailCall body
    Opt.Case _ _ decider jumps ->
      decidecHasTailCall decider || any (hasTailCall . snd) jumps
    _ ->
      False

decidecHasTailCall :: Opt.Decider Opt.Choice -> Bool
decidecHasTailCall decider =
  case decider of
    Opt.Leaf choice ->
      case choice of
        Opt.Inline expr ->
          hasTailCall expr
        Opt.Jump _ ->
          False
    Opt.Chain _ success failure ->
      decidecHasTailCall success || decidecHasTailCall failure
    Opt.FanOut _ tests fallback ->
      decidecHasTailCall fallback || any (decidecHasTailCall . snd) tests
