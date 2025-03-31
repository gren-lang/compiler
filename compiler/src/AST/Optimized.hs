{-# OPTIONS_GHC -Wall #-}

module AST.Optimized
  ( Def (..),
    Expr (..),
    Global (..),
    Path (..),
    Destructor (..),
    Decider (..),
    Choice (..),
    GlobalGraph (..),
    LocalGraph (..),
    Main (..),
    Node (..),
    EffectsType (..),
    empty,
    addGlobalGraph,
    addLocalGraph,
    addKernel,
    toKernelGlobal,
  )
where

import AST.Canonical qualified as Can
import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.Index qualified as Index
import Data.Map qualified as Map
import Data.Name (Name)
import Data.Name qualified as Name
import Data.Set qualified as Set
import Gren.Float qualified as EF
import Gren.Kernel qualified as K
import Gren.ModuleName qualified as ModuleName
import Gren.Package qualified as Pkg
import Gren.String qualified as ES
import Optimize.DecisionTree qualified as DT
import Reporting.Annotation qualified as A

-- EXPRESSIONS

data Expr
  = Bool A.Region Bool
  | Chr A.Region ES.String
  | Str A.Region ES.String
  | Int A.Region Int
  | Float A.Region EF.Float
  | VarLocal A.Region Name
  | VarGlobal A.Region Global
  | VarEnum A.Region Global Index.ZeroBased
  | VarBox A.Region Global
  | VarCycle A.Region ModuleName.Canonical Name
  | VarDebug A.Region Name ModuleName.Canonical (Maybe Name)
  | VarKernel A.Region Name Name
  | Array A.Region [Expr]
  | Function A.Region [A.Located Name] Expr
  | Call A.Region Expr [Expr]
  | TailCall Name [(Name, Expr)]
  | If [(Expr, Expr)] Expr
  | Let Def Expr
  | Destruct Destructor Expr
  | Case Name Name (Decider Choice) [(Int, Expr)]
  | Accessor A.Region Name
  | Access Expr A.Region Name
  | Update A.Region Expr (Map.Map (A.Located Name) Expr)
  | Record A.Region (Map.Map (A.Located Name) Expr)
  deriving (Show)

data Global = Global ModuleName.Canonical Name
  deriving (Show)

-- DEFINITIONS

data Def
  = Def A.Region Name Expr
  | TailDef A.Region Name [A.Located Name] Expr
  deriving (Show)

data Destructor
  = Destructor Name Path
  deriving (Show)

data Path
  = Index Index.ZeroBased Path
  | ArrayIndex Index.ZeroBased Path
  | Field Name Path
  | Unbox Path
  | Root Name
  deriving (Show)

-- BRANCHING

data Decider a
  = Leaf a
  | Chain
      { _testChain :: [(DT.Path, DT.Test)],
        _success :: Decider a,
        _failure :: Decider a
      }
  | FanOut
      { _path :: DT.Path,
        _tests :: [(DT.Test, Decider a)],
        _fallback :: Decider a
      }
  deriving (Show, Eq)

data Choice
  = Inline Expr
  | Jump Int
  deriving (Show)

-- OBJECT GRAPH

data GlobalGraph = GlobalGraph
  { _g_nodes :: Map.Map Global Node,
    _g_fields :: Map.Map Name Int
  }

data LocalGraph = LocalGraph
  { _l_main :: Maybe Main,
    _l_nodes :: Map.Map Global Node, -- PERF profile switching Global to Name
    _l_fields :: Map.Map Name Int
  }

data Main
  = StaticString
  | StaticVDom
  | Dynamic
      { _message :: Can.Type,
        _decoder :: Expr
      }

data Node
  = Define A.Region Expr (Set.Set Global)
  | DefineTailFunc A.Region [A.Located Name] Expr (Set.Set Global)
  | Ctor Index.ZeroBased Int
  | Enum Index.ZeroBased
  | Box
  | Link Global
  | Cycle [Name] [(Name, Expr)] [Def] (Set.Set Global)
  | Manager EffectsType
  | Kernel [K.Chunk] (Set.Set Global)
  | PortIncoming Expr (Set.Set Global)
  | PortOutgoing Expr (Set.Set Global)
  | PortTask (Maybe Expr) Expr (Set.Set Global)

data EffectsType = Cmd | Sub | Fx

-- GRAPHS

empty :: GlobalGraph
empty =
  GlobalGraph Map.empty Map.empty

addGlobalGraph :: GlobalGraph -> GlobalGraph -> GlobalGraph
addGlobalGraph (GlobalGraph nodes1 fields1) (GlobalGraph nodes2 fields2) =
  GlobalGraph
    { _g_nodes = Map.union nodes1 nodes2,
      _g_fields = Map.union fields1 fields2
    }

addLocalGraph :: LocalGraph -> GlobalGraph -> GlobalGraph
addLocalGraph (LocalGraph _ nodes1 fields1) (GlobalGraph nodes2 fields2) =
  GlobalGraph
    { _g_nodes = Map.union nodes1 nodes2,
      _g_fields = Map.union fields1 fields2
    }

addKernel :: Name.Name -> [K.Chunk] -> GlobalGraph -> GlobalGraph
addKernel shortName chunks (GlobalGraph nodes fields) =
  let global = toKernelGlobal shortName
      node = Kernel chunks (foldr addKernelDep Set.empty chunks)
   in GlobalGraph
        { _g_nodes = Map.insert global node nodes,
          _g_fields = Map.union (K.countFields chunks) fields
        }

addKernelDep :: K.Chunk -> Set.Set Global -> Set.Set Global
addKernelDep chunk deps =
  case chunk of
    K.JS _ -> deps
    K.GrenVar home name -> Set.insert (Global home name) deps
    K.JsVar shortName _ -> Set.insert (toKernelGlobal shortName) deps
    K.GrenField _ -> deps
    K.JsField _ -> deps
    K.JsEnum _ -> deps
    K.Debug -> deps
    K.Prod -> deps

toKernelGlobal :: Name.Name -> Global
toKernelGlobal shortName =
  Global (ModuleName.Canonical Pkg.kernel shortName) Name.dollar

-- INSTANCES

instance Eq Global where
  (==) (Global home1 name1) (Global home2 name2) =
    name1 == name2 && home1 == home2

instance Ord Global where
  compare (Global home1 name1) (Global home2 name2) =
    case compare name1 name2 of
      LT -> LT
      EQ -> compare home1 home2
      GT -> GT

-- BINARY

instance Binary Global where
  get = liftM2 Global get get
  put (Global a b) = put a >> put b

instance Binary Expr where
  put expr =
    case expr of
      Bool a b -> putWord8 0 >> put a >> put b
      Chr a b -> putWord8 1 >> put a >> put b
      Str a b -> putWord8 2 >> put a >> put b
      Int a b -> putWord8 3 >> put a >> put b
      Float a b -> putWord8 4 >> put a >> put b
      VarLocal a b -> putWord8 5 >> put a >> put b
      VarGlobal a b -> putWord8 6 >> put a >> put b
      VarEnum a b c -> putWord8 7 >> put a >> put b >> put c
      VarBox a b -> putWord8 8 >> put a >> put b
      VarCycle a b c -> putWord8 9 >> put a >> put b >> put c
      VarDebug a b c d -> putWord8 10 >> put a >> put b >> put c >> put d
      VarKernel a b c -> putWord8 11 >> put a >> put b >> put c
      Array a b -> putWord8 12 >> put a >> put b
      Function a b c -> putWord8 13 >> put a >> put b >> put c
      Call a b c -> putWord8 14 >> put a >> put b >> put c
      TailCall a b -> putWord8 15 >> put a >> put b
      If a b -> putWord8 16 >> put a >> put b
      Let a b -> putWord8 17 >> put a >> put b
      Destruct a b -> putWord8 18 >> put a >> put b
      Case a b c d -> putWord8 19 >> put a >> put b >> put c >> put d
      Accessor a b -> putWord8 20 >> put a >> put b
      Access a b c -> putWord8 21 >> put a >> put b >> put c
      Update a b c -> putWord8 22 >> put a >> put b >> put c
      Record a b -> putWord8 23 >> put a >> put b

  get =
    do
      word <- getWord8
      case word of
        0 -> liftM2 Bool get get
        1 -> liftM2 Chr get get
        2 -> liftM2 Str get get
        3 -> liftM2 Int get get
        4 -> liftM2 Float get get
        5 -> liftM2 VarLocal get get
        6 -> liftM2 VarGlobal get get
        7 -> liftM3 VarEnum get get get
        8 -> liftM2 VarBox get get
        9 -> liftM3 VarCycle get get get
        10 -> liftM4 VarDebug get get get get
        11 -> liftM3 VarKernel get get get
        12 -> liftM2 Array get get
        13 -> liftM3 Function get get get
        14 -> liftM3 Call get get get
        15 -> liftM2 TailCall get get
        16 -> liftM2 If get get
        17 -> liftM2 Let get get
        18 -> liftM2 Destruct get get
        19 -> liftM4 Case get get get get
        20 -> liftM2 Accessor get get
        21 -> liftM3 Access get get get
        22 -> liftM3 Update get get get
        23 -> liftM2 Record get get
        _ -> fail "problem getting Opt.Expr binary"

instance Binary Def where
  put def =
    case def of
      Def a b c -> putWord8 0 >> put a >> put b >> put c
      TailDef a b c d -> putWord8 1 >> put a >> put b >> put c >> put d

  get =
    do
      word <- getWord8
      case word of
        0 -> liftM3 Def get get get
        1 -> liftM4 TailDef get get get get
        _ -> fail "problem getting Opt.Def binary"

instance Binary Destructor where
  get = liftM2 Destructor get get
  put (Destructor a b) = put a >> put b

instance Binary Path where
  put destructor =
    case destructor of
      Index a b -> putWord8 0 >> put a >> put b
      ArrayIndex a b -> putWord8 1 >> put a >> put b
      Field a b -> putWord8 2 >> put a >> put b
      Unbox a -> putWord8 3 >> put a
      Root a -> putWord8 4 >> put a

  get =
    do
      word <- getWord8
      case word of
        0 -> liftM2 Index get get
        1 -> liftM2 ArrayIndex get get
        2 -> liftM2 Field get get
        3 -> liftM Unbox get
        4 -> liftM Root get
        _ -> fail "problem getting Opt.Path binary"

instance (Binary a) => Binary (Decider a) where
  put decider =
    case decider of
      Leaf a -> putWord8 0 >> put a
      Chain a b c -> putWord8 1 >> put a >> put b >> put c
      FanOut a b c -> putWord8 2 >> put a >> put b >> put c

  get =
    do
      word <- getWord8
      case word of
        0 -> liftM Leaf get
        1 -> liftM3 Chain get get get
        2 -> liftM3 FanOut get get get
        _ -> fail "problem getting Opt.Decider binary"

instance Binary Choice where
  put choice =
    case choice of
      Inline expr -> putWord8 0 >> put expr
      Jump index -> putWord8 1 >> put index

  get =
    do
      word <- getWord8
      case word of
        0 -> liftM Inline get
        1 -> liftM Jump get
        _ -> fail "problem getting Opt.Choice binary"

instance Binary GlobalGraph where
  get = liftM2 GlobalGraph get get
  put (GlobalGraph a b) = put a >> put b

instance Binary LocalGraph where
  get = liftM3 LocalGraph get get get
  put (LocalGraph a b c) = put a >> put b >> put c

instance Binary Main where
  put main =
    case main of
      StaticString -> putWord8 0
      StaticVDom -> putWord8 1
      Dynamic a b -> putWord8 2 >> put a >> put b

  get =
    do
      word <- getWord8
      case word of
        0 -> return StaticString
        1 -> return StaticVDom
        2 -> liftM2 Dynamic get get
        _ -> fail "problem getting Opt.Main binary"

instance Binary Node where
  put node =
    case node of
      Define a b c -> putWord8 0 >> put a >> put b >> put c
      DefineTailFunc a b c d -> putWord8 1 >> put a >> put b >> put c >> put d
      Ctor a b -> putWord8 2 >> put a >> put b
      Enum a -> putWord8 3 >> put a
      Box -> putWord8 4
      Link a -> putWord8 5 >> put a
      Cycle a b c d -> putWord8 6 >> put a >> put b >> put c >> put d
      Manager a -> putWord8 7 >> put a
      Kernel a b -> putWord8 8 >> put a >> put b
      PortIncoming a b -> putWord8 9 >> put a >> put b
      PortOutgoing a b -> putWord8 10 >> put a >> put b
      PortTask a b c -> putWord8 11 >> put a >> put b >> put c

  get =
    do
      word <- getWord8
      case word of
        0 -> liftM3 Define get get get
        1 -> liftM4 DefineTailFunc get get get get
        2 -> liftM2 Ctor get get
        3 -> liftM Enum get
        4 -> return Box
        5 -> liftM Link get
        6 -> liftM4 Cycle get get get get
        7 -> liftM Manager get
        8 -> liftM2 Kernel get get
        9 -> liftM2 PortIncoming get get
        10 -> liftM2 PortOutgoing get get
        11 -> liftM3 PortTask get get get
        _ -> fail "problem getting Opt.Node binary"

instance Binary EffectsType where
  put effectsType =
    case effectsType of
      Cmd -> putWord8 0
      Sub -> putWord8 1
      Fx -> putWord8 2

  get =
    do
      word <- getWord8
      case word of
        0 -> return Cmd
        1 -> return Sub
        2 -> return Fx
        _ -> fail "problem getting Opt.EffectsType binary"
