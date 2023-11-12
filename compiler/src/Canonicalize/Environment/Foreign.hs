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
import Data.Bifunctor qualified as Bifunctor
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
    stateWithSignatures <- foldM (addParameterImport home ifaces) emptyState parameters
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
    Just (I.ImplementationInterface pkg params defs unions aliases binops) ->
      if length params /= length importArgs
        then Result.throw $ Error.ImportIsMissingArguments importRegion name (length params) (length importArgs)
        else case detectNonRootSignatureInImportArgs ifaces rootParamNames importArgs of
          Just err ->
            Result.throw err
          Nothing ->
            let !prefix = maybe name fst maybeAlias
                !unpositionedArgs = (map A.toValue importArgs)
                !home = ModuleName.Canonical pkg name
                -- home = ModuleName.Canonical pkg (specializedHomeName name unpositionedArgs)
                -- TODO: Check the shape of each argument for correctness
                !paramMap = Map.fromList $ zip (map (specializedSignatureName home . fst) params) unpositionedArgs

                !rawTypeInfo =
                  Map.union
                    (Map.mapMaybeWithKey (unionToType home paramMap) unions)
                    (Map.mapMaybeWithKey (aliasToType home paramMap) aliases)

                !vars = Map.map (Env.Specific home) (Map.map (\(Can.Forall freevars t) -> Can.Forall freevars (specializeTypeWithParamMap paramMap t)) defs)
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

addParameterImport :: ModuleName.Canonical -> Map.Map ModuleName.Raw I.Interface -> State -> (A.Located Name.Name, Name.Name) -> Result i w State
addParameterImport currentModule ifaces (State vs ts cs bs qvs qts qcs) (A.At aliasRegion alias, name) =
  case Map.lookup name ifaces of
    Just (I.ImplementationInterface {}) ->
      Result.throw $ Error.ImportedModuleIsNotSignature aliasRegion name
    Just (I.SignatureInterface pkg aliasConstraints valueConstraints) ->
      let !prefix = alias
          !originalHome = ModuleName.Canonical pkg name
          !home = specializedSignatureName currentModule prefix

          addAliasConstraint (I.AliasConstraint aliasName) acc =
            Map.insert aliasName (Env.Specific home $ Env.AliasConstraint home aliasName) acc

          mapValueConstraint (I.ValueConstraint _ tipe) =
            Env.Specific home $ Type.annotationFromType $ replaceModuleNameInType originalHome home tipe

          !acs = foldr addAliasConstraint Map.empty aliasConstraints
          !vcs = Map.map mapValueConstraint valueConstraints

          !ts2 = addExposed ts acs
          !vs2 = addExposed vs vcs

          !qts2 = addQualified prefix acs qts
          !qvs2 = addQualified prefix vcs qvs
       in Result.ok (State vs2 ts2 cs bs qvs2 qts2 qcs)
    Nothing ->
      error $ "Failed to find interface named " ++ show name

specializedHomeName :: Name.Name -> [Name.Name] -> Name.Name
specializedHomeName base args =
  case args of
    [] -> base
    _ ->
      let baseStr = Name.toChars base
          argStr = unwords $ List.intersperse ", " $ map Name.toChars args
       in Name.fromChars $ baseStr ++ "(" ++ argStr ++ ")"

specializedSignatureName :: ModuleName.Canonical -> Name.Name -> ModuleName.Canonical
specializedSignatureName base alias =
  let modNameChrs = Name.toChars $ ModuleName._module base
      newModNameChrs = modNameChrs ++ "$" ++ Name.toChars alias
   in base {ModuleName._module = Name.fromChars newModNameChrs}

replaceModuleNameInType :: ModuleName.Canonical -> ModuleName.Canonical -> Can.Type -> Can.Type
replaceModuleNameInType old new tipe =
  case tipe of
    Can.TLambda left right -> Can.TLambda (replaceModuleNameInType old new left) (replaceModuleNameInType old new right)
    Can.TVar _ -> tipe
    Can.TType candidate name tipes ->
      if candidate == old
        then Can.TType new name (map (replaceModuleNameInType old new) tipes)
        else tipe
    Can.TRecord {} -> tipe
    Can.TAlias candidate name fields aliasTipe ->
      if candidate /= old
        then tipe
        else
          Can.TAlias
            new
            name
            (map (\(n, t) -> (n, replaceModuleNameInType old new t)) fields)
            ( case aliasTipe of
                Can.Holey t -> Can.Holey $ replaceModuleNameInType old new t
                Can.Filled t -> Can.Filled $ replaceModuleNameInType old new t
            )
    Can.TAliasConstraint candidate name ->
      if candidate == old
        then Can.TAliasConstraint new name
        else tipe

addExposed :: Env.Exposed a -> Env.Exposed a -> Env.Exposed a
addExposed =
  Map.unionWith Env.mergeInfo

addQualified :: Name.Name -> Env.Exposed a -> Env.Qualified a -> Env.Qualified a
addQualified prefix exposed qualified =
  Map.insertWith addExposed prefix exposed qualified

-- UNION

type ParameterMap = Map.Map ModuleName.Canonical Name.Name

unionToType :: ModuleName.Canonical -> ParameterMap -> Name.Name -> I.Union -> Maybe (Env.Type, Env.Exposed Env.Ctor)
unionToType home paramMap name union =
  unionToTypeHelp home paramMap name <$> I.toPublicUnion union

unionToTypeHelp :: ModuleName.Canonical -> ParameterMap -> Name.Name -> Can.Union -> (Env.Type, Env.Exposed Env.Ctor)
unionToTypeHelp home paramMap name union@(Can.Union vars ctors _ _) =
  let addCtor dict (Can.Ctor ctor index _ args) =
        Map.insert ctor (Env.Specific home (Env.Ctor home name union index (map (specializeTypeWithParamMap paramMap) args))) dict
   in ( Env.Union (length vars) home,
        List.foldl' addCtor Map.empty ctors
      )

-- ALIAS

aliasToType :: ModuleName.Canonical -> ParameterMap -> Name.Name -> I.Alias -> Maybe (Env.Type, Env.Exposed Env.Ctor)
aliasToType home paramMap name alias =
  aliasToTypeHelp home paramMap name <$> I.toPublicAlias alias

aliasToTypeHelp :: ModuleName.Canonical -> ParameterMap -> Name.Name -> Can.Alias -> (Env.Type, Env.Exposed Env.Ctor)
aliasToTypeHelp home paramMap _ (Can.Alias vars tipe) =
  (Env.Alias (length vars) home vars (specializeTypeWithParamMap paramMap tipe), Map.empty)

specializeTypeWithParamMap :: ParameterMap -> Can.Type -> Can.Type
specializeTypeWithParamMap paramMap t =
  case t of
    Can.TLambda left right ->
      Can.TLambda
        (specializeTypeWithParamMap paramMap left)
        (specializeTypeWithParamMap paramMap right)
    Can.TVar _ ->
      t
    Can.TType candidate name tipes ->
      case Map.lookup candidate paramMap of
        Nothing -> t
        Just newName ->
          Can.TType
            (candidate {ModuleName._module = newName})
            name
            (map (specializeTypeWithParamMap paramMap) tipes)
    Can.TRecord fields name ->
      Can.TRecord
        (Map.map (\(Can.FieldType w tipe) -> Can.FieldType w (specializeTypeWithParamMap paramMap tipe)) fields)
        name
    Can.TAlias candidate name fields aliasTipe ->
      case Map.lookup candidate paramMap of
        Nothing -> t
        Just newName ->
          Can.TAlias
            (candidate {ModuleName._module = newName}) -- TODO: ParameterMap should be module -> module (not module -> name)
            name
            (map (Bifunctor.second (specializeTypeWithParamMap paramMap)) fields)
            ( case aliasTipe of
                Can.Filled tipe -> Can.Filled $ specializeTypeWithParamMap paramMap tipe
                Can.Holey tipe -> Can.Holey $ specializeTypeWithParamMap paramMap tipe
            )
    Can.TAliasConstraint _ _ ->
      t

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
                Env.Alias {} ->
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
                Env.Alias {} ->
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
