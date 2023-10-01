{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Canonicalize.Environment.Foreign
  ( createInitialEnv,
  )
where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Canonicalize.Environment qualified as Env
import Canonicalize.Type qualified as Type
import Control.Monad (foldM)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Name qualified as Name
import Gren.Interface qualified as I
import Gren.ModuleName qualified as ModuleName
import Gren.Package qualified as Pkg
import Reporting.Annotation qualified as A
import Reporting.Error.Canonicalize qualified as Error
import Reporting.Result qualified as Result

-- RESULT

type Result i w a =
  Result.Result i w Error.Error a

createInitialEnv :: ModuleName.Canonical -> Map.Map ModuleName.Raw I.Interface -> [(A.Located Name.Name, Name.Name)] -> [Src.Import] -> Result i w Env.Env
createInitialEnv home ifaces parameters imports =
  do
    let paramNames = map (A.toValue . fst) parameters
    stateWithSignatures <- foldM (addParameterImport ifaces) emptyState parameters
    (State vs ts cs bs qvs qts qcs) <- foldM (addImplementationImport ifaces paramNames) stateWithSignatures (toSafeImports home imports)
    Result.ok (Env.Env home (Map.map infoToVar vs) ts cs bs qvs qts qcs)

infoToVar :: Env.Info Can.Annotation -> Env.Var
infoToVar info =
  case info of
    Env.Specific home tipe -> Env.Foreign home tipe
    Env.Ambiguous h hs -> Env.Foreigns h hs

-- STATE

data State = State
  { _vars :: Env.Exposed Can.Annotation,
    _types :: Env.Exposed Env.Type,
    _ctors :: Env.Exposed Env.Ctor,
    _binops :: Env.Exposed Env.Binop,
    _q_vars :: Env.Qualified Can.Annotation,
    _q_types :: Env.Qualified Env.Type,
    _q_ctors :: Env.Qualified Env.Ctor
  }

emptyState :: State
emptyState =
  State Map.empty emptyTypes Map.empty Map.empty Map.empty Map.empty Map.empty

emptyTypes :: Env.Exposed Env.Type
emptyTypes =
  Map.empty

-- TO SAFE IMPORTS

toSafeImports :: ModuleName.Canonical -> [Src.Import] -> [Src.Import]
toSafeImports (ModuleName.Canonical pkg _) imports =
  if Pkg.isKernel pkg
    then filter isNormal imports
    else imports

isNormal :: Src.Import -> Bool
isNormal (Src.Import (A.At _ name) _ maybeAlias _ _ _) =
  if Name.isKernel name
    then case maybeAlias of
      Nothing -> False
      Just _ -> error "kernel imports cannot use `as`"
    else True

-- ADD IMPORTS

addImplementationImport :: Map.Map ModuleName.Raw I.Interface -> [Name.Name] -> State -> Src.Import -> Result i w State
addImplementationImport ifaces rootParamNames (State vs ts cs bs qvs qts qcs) (Src.Import (A.At importRegion name) importArgs maybeAlias exposing _ _) =
  case Map.lookup name ifaces of
    Just (I.ImplementationInterface pkg _params defs unions aliases binops) ->
      case detectNonRootSignatureInImportArgs ifaces rootParamNames importArgs of
        Just err ->
          Result.throw err
        Nothing ->
          let !prefix = maybe name fst maybeAlias
              !home = ModuleName.Canonical pkg name

              !rawTypeInfo =
                Map.union
                  (Map.mapMaybeWithKey (unionToType home) unions)
                  (Map.mapMaybeWithKey (aliasToType home) aliases)

              !vars = Map.map (Env.Specific home) defs
              !types = Map.map (Env.Specific home . fst) rawTypeInfo
              !ctors = Map.foldr (addExposed . snd) Map.empty rawTypeInfo

              !qvs2 = addQualified prefix vars qvs
              !qts2 = addQualified prefix types qts
              !qcs2 = addQualified prefix ctors qcs
           in case exposing of
                Src.Open ->
                  let !vs2 = addExposed vs vars
                      !ts2 = addExposed ts types
                      !cs2 = addExposed cs ctors
                      !bs2 = addExposed bs (Map.mapWithKey (binopToBinop home) binops)
                   in Result.ok (State vs2 ts2 cs2 bs2 qvs2 qts2 qcs2)
                Src.Explicit exposedList ->
                  foldM
                    (addExposedValue home vars rawTypeInfo binops)
                    (State vs ts cs bs qvs2 qts2 qcs2)
                    exposedList
    Just (I.SignatureInterface {}) ->
      Result.throw $ Error.ImportedSignatureWhenNotExpected importRegion name
    Nothing ->
      error $ "Failed to find interface named " ++ show name

detectNonRootSignatureInImportArgs :: Map.Map ModuleName.Raw I.Interface -> [Name.Name] -> [A.Located Name.Name] -> Maybe Error.Error
detectNonRootSignatureInImportArgs ifaces rootParamNames importArgs =
  case importArgs of
    [] ->
      Nothing
    arg@(A.At region argName) : otherArgs ->
      case Map.lookup argName ifaces of
        Just (I.ImplementationInterface {}) ->
          detectNonRootSignatureInImportArgs ifaces rootParamNames otherArgs
        Just (I.SignatureInterface {}) ->
          Just $ Error.ImportArgumentIsSignatureModule arg
        Nothing ->
          if List.elem argName rootParamNames
            then detectNonRootSignatureInImportArgs ifaces rootParamNames otherArgs
            else Just $ Error.ImportNotFound region argName []

addParameterImport :: Map.Map ModuleName.Raw I.Interface -> State -> (A.Located Name.Name, Name.Name) -> Result i w State
addParameterImport ifaces (State vs ts cs bs qvs qts qcs) (A.At aliasRegion alias, name) =
  case Map.lookup name ifaces of
    Just (I.ImplementationInterface {}) ->
      Result.throw $ Error.ImportedModuleIsNotSignature aliasRegion name
    Just (I.SignatureInterface pkg aliasConstraints valueConstraints) ->
      let !prefix = alias
          !home = ModuleName.Canonical pkg name

          addAliasConstraint (I.AliasConstraint aliasName) acc =
            Map.insert aliasName (Env.Specific home $ Env.AliasConstraint home aliasName) acc

          mapValueConstraint (I.ValueConstraint _ tipe) =
            Env.Specific home $ Type.annotationFromType tipe

          !acs = foldr addAliasConstraint Map.empty aliasConstraints
          !vcs = Map.map mapValueConstraint valueConstraints

          !ts2 = addExposed ts acs
          !vs2 = addExposed vs vcs

          !qts2 = addQualified prefix acs qts
          !qvs2 = addQualified prefix vcs qvs
       in Result.ok (State vs2 ts2 cs bs qvs2 qts2 qcs)
    Nothing ->
      error $ "Failed to find interface named " ++ show name

addExposed :: Env.Exposed a -> Env.Exposed a -> Env.Exposed a
addExposed =
  Map.unionWith Env.mergeInfo

addQualified :: Name.Name -> Env.Exposed a -> Env.Qualified a -> Env.Qualified a
addQualified prefix exposed qualified =
  Map.insertWith addExposed prefix exposed qualified

-- UNION

unionToType :: ModuleName.Canonical -> Name.Name -> I.Union -> Maybe (Env.Type, Env.Exposed Env.Ctor)
unionToType home name union =
  unionToTypeHelp home name <$> I.toPublicUnion union

unionToTypeHelp :: ModuleName.Canonical -> Name.Name -> Can.Union -> (Env.Type, Env.Exposed Env.Ctor)
unionToTypeHelp home name union@(Can.Union vars ctors _ _) =
  let addCtor dict (Can.Ctor ctor index _ args) =
        Map.insert ctor (Env.Specific home (Env.Ctor home name union index args)) dict
   in ( Env.Union (length vars) home,
        List.foldl' addCtor Map.empty ctors
      )

-- ALIAS

aliasToType :: ModuleName.Canonical -> Name.Name -> I.Alias -> Maybe (Env.Type, Env.Exposed Env.Ctor)
aliasToType home name alias =
  aliasToTypeHelp home name <$> I.toPublicAlias alias

aliasToTypeHelp :: ModuleName.Canonical -> Name.Name -> Can.Alias -> (Env.Type, Env.Exposed Env.Ctor)
aliasToTypeHelp home _ (Can.Alias vars tipe) =
  (Env.Alias (length vars) home vars tipe, Map.empty)

-- BINOP

binopToBinop :: ModuleName.Canonical -> Name.Name -> I.Binop -> Env.Info Env.Binop
binopToBinop home op (I.Binop name annotation associativity precedence) =
  Env.Specific home (Env.Binop op home name annotation associativity precedence)

-- ADD EXPOSED VALUE

addExposedValue ::
  ModuleName.Canonical ->
  Env.Exposed Can.Annotation ->
  Map.Map Name.Name (Env.Type, Env.Exposed Env.Ctor) ->
  Map.Map Name.Name I.Binop ->
  State ->
  Src.Exposed ->
  Result i w State
addExposedValue home vars types binops (State vs ts cs bs qvs qts qcs) exposed =
  case exposed of
    Src.Lower (A.At region name) ->
      case Map.lookup name vars of
        Just info ->
          Result.ok (State (Map.insertWith Env.mergeInfo name info vs) ts cs bs qvs qts qcs)
        Nothing ->
          Result.throw (Error.ImportExposingNotFound region home name (Map.keys vars))
    Src.Upper (A.At region name) privacy ->
      case privacy of
        Src.Private ->
          case Map.lookup name types of
            Just (tipe, ctors) ->
              case tipe of
                Env.Union _ _ ->
                  let !ts2 = Map.insert name (Env.Specific home tipe) ts
                   in Result.ok (State vs ts2 cs bs qvs qts qcs)
                Env.Alias _ _ _ _ ->
                  let !ts2 = Map.insert name (Env.Specific home tipe) ts
                      !cs2 = addExposed cs ctors
                   in Result.ok (State vs ts2 cs2 bs qvs qts qcs)
                Env.AliasConstraint _ _ ->
                  Result.throw $ Error.ImportExposingNotFound region home name (Map.keys types)
            Nothing ->
              case checkForCtorMistake name types of
                tipe : _ ->
                  Result.throw $ Error.ImportCtorByName region name tipe
                [] ->
                  Result.throw $ Error.ImportExposingNotFound region home name (Map.keys types)
        Src.Public dotDotRegion ->
          case Map.lookup name types of
            Just (tipe, ctors) ->
              case tipe of
                Env.Union _ _ ->
                  let !ts2 = Map.insert name (Env.Specific home tipe) ts
                      !cs2 = addExposed cs ctors
                   in Result.ok (State vs ts2 cs2 bs qvs qts qcs)
                Env.Alias _ _ _ _ ->
                  Result.throw (Error.ImportOpenAlias dotDotRegion name)
                Env.AliasConstraint _ _ ->
                  Result.throw $ Error.ImportExposingNotFound region home name (Map.keys types)
            Nothing ->
              Result.throw (Error.ImportExposingNotFound region home name (Map.keys types))
    Src.Operator region op ->
      case Map.lookup op binops of
        Just binop ->
          let !bs2 = Map.insert op (binopToBinop home op binop) bs
           in Result.ok (State vs ts cs bs2 qvs qts qcs)
        Nothing ->
          Result.throw (Error.ImportExposingNotFound region home op (Map.keys binops))

checkForCtorMistake :: Name.Name -> Map.Map Name.Name (Env.Type, Env.Exposed Env.Ctor) -> [Name.Name]
checkForCtorMistake givenName types =
  Map.foldr addMatches [] types
  where
    addMatches (_, exposedCtors) matches =
      Map.foldrWithKey addMatch matches exposedCtors

    addMatch ctorName info matches =
      if ctorName /= givenName
        then matches
        else case info of
          Env.Specific _ (Env.Ctor _ tipeName _ _ _) ->
            tipeName : matches
          Env.Ambiguous _ _ ->
            matches
