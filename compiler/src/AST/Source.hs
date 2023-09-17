{-# OPTIONS_GHC -Wall #-}

module AST.Source
  ( Comment,
    Comment_ (..),
    GREN_COMMENT,
    Expr,
    Expr_ (..),
    VarType (..),
    ArrayEntry,
    BinopsSegment,
    IfBranch,
    CaseBranch,
    RecordField,
    Def (..),
    Pattern,
    Pattern_ (..),
    RecordFieldPattern,
    RecordFieldPattern_ (..),
    PArrayEntry,
    Type,
    Type_ (..),
    TRecordField,
    SourceOrder,
    Module (..),
    getName,
    getModuleImports,
    getImportName,
    Import (..),
    Value (..),
    Union (..),
    UnionVariant,
    Alias (..),
    AliasConstraint (..),
    ValueConstraint (..),
    Infix (..),
    Port (..),
    Effects (..),
    Manager (..),
    Docs (..),
    DocComment (..),
    Exposing (..),
    Exposed (..),
    Privacy (..),
  )
where

import AST.SourceComments (Comment, Comment_, GREN_COMMENT)
import AST.SourceComments qualified as SC
import AST.Utils.Binop qualified as Binop
import Data.List.NonEmpty (NonEmpty)
import Data.Name (Name)
import Data.Name qualified as Name
import Gren.Float qualified as EF
import Gren.Int qualified as GI
import Gren.String qualified as ES
import Parse.Primitives qualified as P
import Reporting.Annotation qualified as A

-- EXPRESSIONS

type Expr = A.Located Expr_

data Expr_
  = Chr ES.String
  | Str ES.String ES.StringFormat
  | Int Int GI.IntFormat
  | Float EF.Float
  | Var VarType Name
  | VarQual VarType Name Name
  | Array [ArrayEntry]
  | Op Name
  | Negate Expr
  | Binops [BinopsSegment] Expr
  | Lambda [([Comment], Pattern)] Expr SC.LambdaComments
  | Call Expr [([Comment], Expr)]
  | If [IfBranch] Expr SC.IfComments
  | Let [([Comment], A.Located Def)] Expr SC.LetComments
  | Case Expr [CaseBranch] SC.CaseComments
  | Accessor Name
  | Access Expr (A.Located Name)
  | Update Expr [RecordField] SC.UpdateComments
  | Record [RecordField]
  | Parens [Comment] Expr [Comment]
  deriving (Show)

data VarType = LowVar | CapVar
  deriving (Show)

type ArrayEntry =
  (Expr, SC.ArrayEntryComments)

type BinopsSegment =
  (Expr, A.Located Name, SC.BinopsSegmentComments)

type IfBranch =
  (Expr, Expr, SC.IfBranchComments)

type CaseBranch =
  (Pattern, Expr, SC.CaseBranchComments)

type RecordField =
  (A.Located Name, Expr, SC.RecordFieldComments)

-- DEFINITIONS

data Def
  = Define (A.Located Name) [([Comment], Pattern)] Expr (Maybe (Type, SC.ValueTypeComments)) SC.ValueComments
  | Destruct Pattern Expr SC.ValueComments
  deriving (Show)

-- PATTERN

type Pattern = A.Located Pattern_

data Pattern_
  = PAnything Name
  | PVar Name
  | PRecord [RecordFieldPattern]
  | PAlias Pattern (A.Located Name)
  | PCtor A.Region Name [([Comment], Pattern)]
  | PCtorQual A.Region Name Name [([Comment], Pattern)]
  | PArray [PArrayEntry]
  | PChr ES.String
  | PStr ES.String
  | PInt Int GI.IntFormat
  deriving (Show)

type RecordFieldPattern = A.Located RecordFieldPattern_

data RecordFieldPattern_ = RFPattern (A.Located Name) Pattern
  deriving (Show)

type PArrayEntry = (Pattern, SC.PArrayEntryComments)

-- TYPE

type Type =
  A.Located Type_

data Type_
  = TLambda Type Type SC.TLambdaComments
  | TVar Name
  | TType A.Region Name [([Comment], Type)]
  | TTypeQual A.Region Name Name [([Comment], Type)]
  | TRecord [TRecordField] (Maybe (A.Located Name, SC.UpdateComments))
  | TParens Type SC.TParensComments
  deriving (Show)

type TRecordField = (A.Located Name, Type, SC.RecordFieldComments)

-- MODULE

type SourceOrder = Int

data Module
  = ImplementationModule
      { _name :: Maybe (A.Located Name),
        _params :: [(A.Located Name, A.Located Name)],
        _exports :: A.Located Exposing,
        _docs :: Docs,
        _imports :: [([Comment], Import)],
        _values :: [(SourceOrder, A.Located Value)],
        _unions :: [(SourceOrder, A.Located Union)],
        _aliases :: [(SourceOrder, A.Located Alias)],
        _binops :: ([Comment], [A.Located Infix]),
        _topLevelComments :: [(SourceOrder, NonEmpty Comment)],
        _headerComments :: SC.HeaderComments,
        _effects :: Effects
      }
  | SignatureModule
      { _sm_name :: A.Located Name,
        _sm_docs :: Docs,
        _sm_imports :: [([Comment], Import)],
        _sm_aliasConstraints :: [(SourceOrder, A.Located AliasConstraint)],
        _sm_valueConstraints :: [(SourceOrder, A.Located ValueConstraint)]
      }
  deriving (Show)

getName :: Module -> Name
getName modul =
  case modul of
    ImplementationModule maybeName _ _ _ _ _ _ _ _ _ _ _ ->
      case maybeName of
        Just (A.At _ name) ->
          name
        Nothing ->
          Name._Main
    SignatureModule (A.At _ name) _ _ _ _ ->
      name

getModuleImports :: Module -> [([Comment], Import)]
getModuleImports modul =
  case modul of
    ImplementationModule {_params, _imports} ->
      let paramsAsImports =
            map paramToImport _params

          paramToImport :: (A.Located Name, A.Located Name) -> ([Comment], Import)
          paramToImport (argName, signatureName) =
            ( [],
              Import
                { _import = signatureName,
                  _args = [],
                  _alias = Just (A.toValue argName, SC.ImportAliasComments [] []),
                  _exposing = Open,
                  _exposingComments = Nothing,
                  _importComments = SC.ImportComments [] []
                }
            )
       in paramsAsImports ++ _imports
    SignatureModule {_sm_imports} ->
      _sm_imports

getImportName :: Import -> Name
getImportName (Import (A.At _ name) _ _ _ _ _) =
  name

data Import = Import
  { _import :: A.Located Name,
    _args :: [A.Located Name],
    _alias :: Maybe (Name, SC.ImportAliasComments),
    _exposing :: Exposing,
    _exposingComments :: Maybe SC.ImportExposingComments,
    _importComments :: SC.ImportComments
  }
  deriving (Show)

data Value = Value (A.Located Name) [([Comment], Pattern)] Expr (Maybe (Type, SC.ValueTypeComments)) SC.ValueComments
  deriving (Show)

data Union = Union (A.Located Name) [([Comment], A.Located Name)] [UnionVariant] SC.UnionComments
  deriving (Show)

type UnionVariant =
  ([Comment], A.Located Name, [([Comment], Type)], [Comment])

data Alias = Alias (A.Located Name) [A.Located Name] Type
  deriving (Show)

data AliasConstraint = AliasConstraint (A.Located Name)
  deriving (Show)

data ValueConstraint = ValueConstraint (A.Located Name) Type
  deriving (Show)

data Infix = Infix Name Binop.Associativity Binop.Precedence Name
  deriving (Show)

data Port = Port (A.Located Name) Type
  deriving (Show)

data Effects
  = NoEffects
  | Ports [(SourceOrder, Port)] SC.PortsComments
  | Manager A.Region Manager SC.ManagerComments
  deriving (Show)

data Manager
  = Cmd (A.Located Name) SC.CmdComments
  | Sub (A.Located Name) SC.SubComments
  | Fx (A.Located Name) (A.Located Name) SC.FxComments
  deriving (Show)

data Docs
  = NoDocs A.Region
  | YesDocs DocComment [(Name, DocComment)]
  deriving (Show)

newtype DocComment
  = DocComment P.Snippet
  deriving (Show)

-- EXPOSING

data Exposing
  = Open
  | Explicit [Exposed]
  deriving (Show)

data Exposed
  = Lower (A.Located Name)
  | Upper (A.Located Name) Privacy
  | Operator A.Region Name
  deriving (Show)

data Privacy
  = Public A.Region
  | Private
  deriving (Show)
