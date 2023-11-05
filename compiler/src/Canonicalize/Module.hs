module Canonicalize.Module
  ( canonicalize,
  )
where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Canonicalize.Effects qualified as Effects
import Canonicalize.Environment qualified as Env
import Canonicalize.Environment.Dups qualified as Dups
import Canonicalize.Environment.Foreign qualified as Foreign
import Canonicalize.Environment.Local qualified as Local
import Canonicalize.Expression qualified as Expr
import Canonicalize.Pattern qualified as Pattern
import Canonicalize.Type qualified as Type
import Data.Bifunctor (bimap)
import Data.Bifunctor qualified as Bifunctor
import Data.Graph qualified as Graph
import Data.Index qualified as Index
import Data.Map qualified as Map
import Data.Name qualified as Name
import Gren.Interface qualified as I
import Gren.ModuleName qualified as ModuleName
import Gren.Package qualified as Pkg
import Reporting.Annotation qualified as A
import Reporting.Error.Canonicalize qualified as Error
import Reporting.Result qualified as Result
import Reporting.Warning qualified as W

-- RESULT

type Result i w a =
  Result.Result i w Error.Error a

-- MODULES

canonicalize :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Result i [W.Warning] Can.Module
canonicalize pkg ifaces modul =
  case modul of
    (Src.ImplementationModule _ parameters exports docs imports valuesWithSourceOrder _ _ (_, binops) _ _ effects) ->
      let values = fmap snd valuesWithSourceOrder
          paramMap = map (bimap A.toValue A.toValue) parameters
          home = ModuleName.Canonical pkg (Src.getName modul)
          cbinops = Map.fromList (map canonicalizeBinop binops)
       in do
            (env, cunions, caliases) <-
              Local.add modul
                =<< Foreign.createInitialEnv home ifaces (map (Bifunctor.second A.toValue) parameters) (fmap snd imports)

            cvalues <- canonicalizeValues env values
            ceffects <- Effects.canonicalize env values cunions effects
            cexports <- canonicalizeExports values cunions caliases cbinops ceffects exports

            return $ Can.ImplementationModule home paramMap cexports docs cvalues cunions caliases cbinops ceffects
    (Src.SignatureModule _ _ imports aliasConstraints valueConstraints) ->
      let home = ModuleName.Canonical pkg (Src.getName modul)
          acs = map ((\(Src.AliasConstraint (A.At _ name)) -> name) . A.toValue . snd) aliasConstraints
          nameAndTypes = map ((\(Src.ValueConstraint (A.At _ name) tipe) -> (name, tipe)) . A.toValue . snd) valueConstraints
       in do
            (env, _, _) <-
              Local.add modul
                =<< Foreign.createInitialEnv home ifaces [] (fmap snd imports)

            vcs <- canonicalizeValueConstraints env nameAndTypes

            return $ Can.SignatureModule home acs vcs

-- CANONICALIZE BINOP

canonicalizeBinop :: A.Located Src.Infix -> (Name.Name, Can.Binop)
canonicalizeBinop (A.At _ (Src.Infix op associativity precedence func)) =
  (op, Can.Binop_ associativity precedence func)

-- DECLARATIONS / CYCLE DETECTION
--
-- There are two phases of cycle detection:
--
-- 1. Detect cycles using ALL dependencies => needed for type inference
-- 2. Detect cycles using DIRECT dependencies => nonterminating recursion
--

canonicalizeValues :: Env.Env -> [A.Located Src.Value] -> Result i [W.Warning] Can.Decls
canonicalizeValues env values =
  do
    nodes <- traverse (toNodeOne env) values
    detectCycles (Graph.stronglyConnComp nodes)

detectCycles :: [Graph.SCC NodeTwo] -> Result i w Can.Decls
detectCycles sccs =
  case sccs of
    [] ->
      Result.ok Can.SaveTheEnvironment
    scc : otherSccs ->
      case scc of
        Graph.AcyclicSCC (def, _, _) ->
          Can.Declare def <$> detectCycles otherSccs
        Graph.CyclicSCC subNodes ->
          do
            defs <- traverse detectBadCycles (Graph.stronglyConnComp subNodes)
            case defs of
              [] -> detectCycles otherSccs
              d : ds -> Can.DeclareRec d ds <$> detectCycles otherSccs

detectBadCycles :: Graph.SCC Can.Def -> Result i w Can.Def
detectBadCycles scc =
  case scc of
    Graph.AcyclicSCC def ->
      Result.ok def
    Graph.CyclicSCC [] ->
      error "The definition of Data.Graph.SCC should not allow empty CyclicSCC!"
    Graph.CyclicSCC (def : defs) ->
      let (A.At region name) = extractDefName def
          names = map (A.toValue . extractDefName) defs
       in Result.throw (Error.RecursiveDecl region name names)

extractDefName :: Can.Def -> A.Located Name.Name
extractDefName def =
  case def of
    Can.Def name _ _ -> name
    Can.TypedDef name _ _ _ _ -> name

-- DECLARATIONS / CYCLE DETECTION SETUP
--
-- toNodeOne and toNodeTwo set up nodes for the two cycle detection phases.
--

-- Phase one nodes track ALL dependencies.
-- This allows us to find cyclic values for type inference.
type NodeOne =
  (NodeTwo, Name.Name, [Name.Name])

-- Phase two nodes track DIRECT dependencies.
-- This allows us to detect cycles that definitely do not terminate.
type NodeTwo =
  (Can.Def, Name.Name, [Name.Name])

toNodeOne :: Env.Env -> A.Located Src.Value -> Result i [W.Warning] NodeOne
toNodeOne env (A.At _ (Src.Value aname@(A.At _ name) srcArgs body maybeType _)) =
  case maybeType of
    Nothing ->
      do
        (args, argBindings) <-
          Pattern.verify (Error.DPFuncArgs name) $
            traverse (Pattern.canonicalize env . snd) srcArgs

        newEnv <-
          Env.addLocals argBindings env

        (cbody, freeLocals) <-
          Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize newEnv body)

        let def = Can.Def aname args cbody
        return
          ( toNodeTwo name srcArgs def freeLocals,
            name,
            Map.keys freeLocals
          )
    Just (srcType, _) ->
      do
        (Can.Forall freeVars tipe) <- Type.toAnnotation env srcType

        ((args, resultType), argBindings) <-
          Pattern.verify (Error.DPFuncArgs name) $
            Expr.gatherTypedArgs env name (fmap snd srcArgs) tipe Index.first []

        newEnv <-
          Env.addLocals argBindings env

        (cbody, freeLocals) <-
          Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize newEnv body)

        let def = Can.TypedDef aname freeVars args cbody resultType
        return
          ( toNodeTwo name srcArgs def freeLocals,
            name,
            Map.keys freeLocals
          )

toNodeTwo :: Name.Name -> [arg] -> Can.Def -> Expr.FreeLocals -> NodeTwo
toNodeTwo name args def freeLocals =
  case args of
    [] ->
      (def, name, Map.foldrWithKey addDirects [] freeLocals)
    _ ->
      (def, name, [])

addDirects :: Name.Name -> Expr.Uses -> [Name.Name] -> [Name.Name]
addDirects name (Expr.Uses directUses _) directDeps =
  if directUses > 0
    then name : directDeps
    else directDeps

-- CANONICALIZE EXPORTS

canonicalizeExports ::
  [A.Located Src.Value] ->
  Map.Map Name.Name union ->
  Map.Map Name.Name alias ->
  Map.Map Name.Name binop ->
  Can.Effects ->
  A.Located Src.Exposing ->
  Result i w Can.Exports
canonicalizeExports values unions aliases binops effects (A.At region exposing) =
  case exposing of
    Src.Open ->
      Result.ok (Can.ExportEverything region)
    Src.Explicit exposeds ->
      do
        let names = Map.fromList (map valueToName values)
        infos <- traverse (checkExposed names unions aliases binops effects) exposeds
        Can.Export <$> Dups.detect Error.ExportDuplicate (Dups.unions infos)

valueToName :: A.Located Src.Value -> (Name.Name, ())
valueToName (A.At _ (Src.Value (A.At _ name) _ _ _ _)) =
  (name, ())

checkExposed ::
  Map.Map Name.Name value ->
  Map.Map Name.Name union ->
  Map.Map Name.Name alias ->
  Map.Map Name.Name binop ->
  Can.Effects ->
  Src.Exposed ->
  Result i w (Dups.Dict (A.Located Can.Export))
checkExposed values unions aliases binops effects exposed =
  case exposed of
    Src.Lower (A.At region name) ->
      if Map.member name values
        then ok name region Can.ExportValue
        else case checkPorts effects name of
          Nothing ->
            ok name region Can.ExportPort
          Just ports ->
            Result.throw $
              Error.ExportNotFound region Error.BadVar name $
                ports ++ Map.keys values
    Src.Operator region name ->
      if Map.member name binops
        then ok name region Can.ExportBinop
        else
          Result.throw $
            Error.ExportNotFound region Error.BadOp name $
              Map.keys binops
    Src.Upper (A.At region name) (Src.Public dotDotRegion) ->
      if Map.member name unions
        then ok name region Can.ExportUnionOpen
        else
          if Map.member name aliases
            then Result.throw $ Error.ExportOpenAlias dotDotRegion name
            else
              Result.throw $
                Error.ExportNotFound region Error.BadType name $
                  Map.keys unions ++ Map.keys aliases
    Src.Upper (A.At region name) Src.Private ->
      if Map.member name unions
        then ok name region Can.ExportUnionClosed
        else
          if Map.member name aliases
            then ok name region Can.ExportAlias
            else
              Result.throw $
                Error.ExportNotFound region Error.BadType name $
                  Map.keys unions ++ Map.keys aliases

checkPorts :: Can.Effects -> Name.Name -> Maybe [Name.Name]
checkPorts effects name =
  case effects of
    Can.NoEffects ->
      Just []
    Can.Ports ports ->
      if Map.member name ports then Nothing else Just (Map.keys ports)
    Can.Manager {} ->
      Just []

ok :: Name.Name -> A.Region -> Can.Export -> Result i w (Dups.Dict (A.Located Can.Export))
ok name region export =
  Result.ok $ Dups.one name region (A.At region export)

-- VALUE CONSTRAINTS

canonicalizeValueConstraints :: Env.Env -> [(Name.Name, Src.Type)] -> Result i [W.Warning] [Can.ValueConstraint]
canonicalizeValueConstraints env pairs =
  mapM (canonicalizeValueConstraintsHelper env) pairs

canonicalizeValueConstraintsHelper :: Env.Env -> (Name.Name, Src.Type) -> Result i [W.Warning] Can.ValueConstraint
canonicalizeValueConstraintsHelper env (name, tipe) = do
  canTipe <- Type.canonicalize env tipe
  Result.ok $ Can.ValueConstraint name canTipe
