{-# OPTIONS_GHC -Wall #-}

module Gren.Interface
  ( Interface (..),
    Union (..),
    Alias (..),
    Binop (..),
    AliasConstraint (..),
    ValueConstraint (..),
    fromModule,
    toPublicUnion,
    toPublicAlias,
    DependencyInterface (..),
    public,
    private,
    privatize,
    extractUnion,
    extractAlias,
  )
where

import AST.Canonical qualified as Can
import AST.Utils.Binop qualified as Binop
import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Data.Binary
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Name qualified as Name
import Gren.ModuleName qualified as ModuleName
import Gren.Package qualified as Pkg
import Reporting.Annotation qualified as A

-- INTERFACE

data Interface
  = ImplementationInterface
      { _home :: Pkg.Name,
        _parameters :: [(Name.Name, ModuleName.Raw)],
        _values :: Map.Map Name.Name Can.Annotation,
        _unions :: Map.Map Name.Name Union,
        _aliases :: Map.Map Name.Name Alias,
        _binops :: Map.Map Name.Name Binop
      }
  | SignatureInterface
      { _si_home :: Pkg.Name,
        _si_aliasConstraints :: [AliasConstraint],
        _si_valueConstraints :: Map.Map Name.Name ValueConstraint
      }
  deriving (Eq, Show)

data Union
  = OpenUnion Can.Union
  | ClosedUnion Can.Union
  | PrivateUnion Can.Union
  deriving (Eq, Show)

data Alias
  = PublicAlias Can.Alias
  | PrivateAlias Can.Alias
  deriving (Eq, Show)

data Binop = Binop
  { _op_name :: Name.Name,
    _op_annotation :: Can.Annotation,
    _op_associativity :: Binop.Associativity,
    _op_precedence :: Binop.Precedence
  }
  deriving (Eq, Show)

newtype AliasConstraint = AliasConstraint Name.Name
  deriving (Eq, Show)

data ValueConstraint = ValueConstraint Name.Name Can.Type
  deriving (Eq, Show)

-- FROM MODULE

fromModule :: Pkg.Name -> Can.Module -> Map.Map Name.Name Can.Annotation -> Interface
fromModule home (Can.ImplementationModule _ params exports _ _ unions aliases binops _) annotations =
  ImplementationInterface
    { _home = home,
      _parameters = params,
      _values = restrict exports annotations,
      _unions = restrictUnions exports unions,
      _aliases = restrictAliases exports aliases,
      _binops = restrict exports (Map.map (toOp annotations) binops)
    }
fromModule home (Can.SignatureModule _ aliasConstraints valueConstraints) _annotation =
  SignatureInterface
    { _si_home = home,
      _si_aliasConstraints = map AliasConstraint aliasConstraints,
      _si_valueConstraints =
        foldl
          (\acc (Can.ValueConstraint name t) -> Map.insert name (ValueConstraint name t) acc)
          Map.empty
          valueConstraints
    }

restrict :: Can.Exports -> Map.Map Name.Name a -> Map.Map Name.Name a
restrict exports dict =
  case exports of
    Can.ExportEverything _ ->
      dict
    Can.Export explicitExports ->
      Map.intersection dict explicitExports

toOp :: Map.Map Name.Name Can.Annotation -> Can.Binop -> Binop
toOp types (Can.Binop_ associativity precedence name) =
  Binop name (types ! name) associativity precedence

restrictUnions :: Can.Exports -> Map.Map Name.Name Can.Union -> Map.Map Name.Name Union
restrictUnions exports unions =
  case exports of
    Can.ExportEverything _ ->
      Map.map OpenUnion unions
    Can.Export explicitExports ->
      Map.merge onLeft onRight onBoth explicitExports unions
      where
        onLeft = Map.dropMissing
        onRight = Map.mapMissing (\_ union -> PrivateUnion union)
        onBoth = Map.zipWithMatched $ \_ (A.At _ export) union ->
          case export of
            Can.ExportUnionOpen -> OpenUnion union
            Can.ExportUnionClosed -> ClosedUnion union
            _ -> error "impossible exports discovered in restrictUnions"

restrictAliases :: Can.Exports -> Map.Map Name.Name Can.Alias -> Map.Map Name.Name Alias
restrictAliases exports aliases =
  case exports of
    Can.ExportEverything _ ->
      Map.map PublicAlias aliases
    Can.Export explicitExports ->
      Map.merge onLeft onRight onBoth explicitExports aliases
      where
        onLeft = Map.dropMissing
        onRight = Map.mapMissing (\_ a -> PrivateAlias a)
        onBoth = Map.zipWithMatched (\_ _ a -> PublicAlias a)

-- TO PUBLIC

toPublicUnion :: Union -> Maybe Can.Union
toPublicUnion iUnion =
  case iUnion of
    OpenUnion union -> Just union
    ClosedUnion (Can.Union vars _ _ opts) -> Just (Can.Union vars [] 0 opts)
    PrivateUnion _ -> Nothing

toPublicAlias :: Alias -> Maybe Can.Alias
toPublicAlias iAlias =
  case iAlias of
    PublicAlias alias -> Just alias
    PrivateAlias _ -> Nothing

-- DEPENDENCY INTERFACE

data DependencyInterface
  = Public Interface
  | Private
      Pkg.Name
      (Map.Map Name.Name Can.Union)
      (Map.Map Name.Name Can.Alias)

public :: Interface -> DependencyInterface
public =
  Public

private :: Interface -> DependencyInterface
private (ImplementationInterface pkg _ _ unions aliases _) =
  Private pkg (Map.map extractUnion unions) (Map.map extractAlias aliases)
private (SignatureInterface pkg _ _) =
  Private pkg Map.empty Map.empty

extractUnion :: Union -> Can.Union
extractUnion iUnion =
  case iUnion of
    OpenUnion union -> union
    ClosedUnion union -> union
    PrivateUnion union -> union

extractAlias :: Alias -> Can.Alias
extractAlias iAlias =
  case iAlias of
    PublicAlias alias -> alias
    PrivateAlias alias -> alias

privatize :: DependencyInterface -> DependencyInterface
privatize di =
  case di of
    Public i -> private i
    Private {} -> di

-- BINARY

instance Binary Interface where
  get = do
    v <- getWord8
    case v of
      1 -> ImplementationInterface <$> get <*> get <*> get <*> get <*> get <*> get
      2 -> liftM3 SignatureInterface get get get
      _ -> fail "binary encoding of Interface was corrupted"

  put (ImplementationInterface a b c d e f) = putWord8 1 >> put a >> put b >> put c >> put d >> put e >> put f
  put (SignatureInterface a b c) = putWord8 2 >> put a >> put b >> put c

instance Binary Union where
  put union =
    case union of
      OpenUnion u -> putWord8 0 >> put u
      ClosedUnion u -> putWord8 1 >> put u
      PrivateUnion u -> putWord8 2 >> put u

  get =
    do
      n <- getWord8
      case n of
        0 -> liftM OpenUnion get
        1 -> liftM ClosedUnion get
        2 -> liftM PrivateUnion get
        _ -> fail "binary encoding of Union was corrupted"

instance Binary Alias where
  put union =
    case union of
      PublicAlias a -> putWord8 0 >> put a
      PrivateAlias a -> putWord8 1 >> put a

  get =
    do
      n <- getWord8
      case n of
        0 -> liftM PublicAlias get
        1 -> liftM PrivateAlias get
        _ -> fail "binary encoding of Alias was corrupted"

instance Binary Binop where
  get =
    liftM4 Binop get get get get

  put (Binop a b c d) =
    put a >> put b >> put c >> put d

instance Binary DependencyInterface where
  put union =
    case union of
      Public a -> putWord8 0 >> put a
      Private a b c -> putWord8 1 >> put a >> put b >> put c

  get =
    do
      n <- getWord8
      case n of
        0 -> liftM Public get
        1 -> liftM3 Private get get get
        _ -> fail "binary encoding of DependencyInterface was corrupted"

instance Binary AliasConstraint where
  put (AliasConstraint a) = put a
  get = liftM AliasConstraint get

instance Binary ValueConstraint where
  put (ValueConstraint a b) = put a >> put b
  get = liftM2 ValueConstraint get get
