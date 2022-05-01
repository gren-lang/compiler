{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -Wall #-}

module AST.Source
  ( Comment (..),
    GREN_COMMENT,
    Expr,
    Expr_ (..),
    VarType (..),
    Def (..),
    Pattern,
    Pattern_ (..),
    RecordFieldPattern,
    RecordFieldPattern_ (..),
    Type,
    Type_ (..),
    Module (..),
    getName,
    getImportName,
    Import (..),
    Value (..),
    Union (..),
    Alias (..),
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

import qualified AST.Utils.Binop as Binop
import Data.Name (Name)
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import qualified Gren.Float as EF
import qualified Gren.String as ES
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A

-- COMMENTS

data Comment
  = BlockComment (Utf8.Utf8 GREN_COMMENT)
  | LineComment ES.String
  | CommentTrickOpener
  | CommentTrickCloser
  | CommentTrickBlock ES.String

data GREN_COMMENT

-- EXPRESSIONS

type Expr = A.Located Expr_

data Expr_
  = Chr ES.String
  | Str ES.String
  | Int Int
  | Float EF.Float
  | Var VarType Name
  | VarQual VarType Name Name
  | Array [Expr]
  | Op Name
  | Negate Expr
  | Binops [(Expr, A.Located Name)] Expr
  | Lambda [Pattern] Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let [A.Located Def] Expr
  | Case Expr [(Pattern, Expr)]
  | Accessor Name
  | Access Expr (A.Located Name)
  | Update (A.Located Name) [(A.Located Name, Expr)]
  | Record [(A.Located Name, Expr)]

data VarType = LowVar | CapVar

-- DEFINITIONS

data Def
  = Define (A.Located Name) [Pattern] Expr (Maybe Type)
  | Destruct Pattern Expr

-- PATTERN

type Pattern = A.Located Pattern_

data Pattern_
  = PAnything
  | PVar Name
  | PRecord [RecordFieldPattern]
  | PAlias Pattern (A.Located Name)
  | PCtor A.Region Name [Pattern]
  | PCtorQual A.Region Name Name [Pattern]
  | PArray [Pattern]
  | PChr ES.String
  | PStr ES.String
  | PInt Int

type RecordFieldPattern = A.Located RecordFieldPattern_

data RecordFieldPattern_ = RFPattern (A.Located Name) Pattern

-- TYPE

type Type =
  A.Located Type_

data Type_
  = TLambda Type Type
  | TVar Name
  | TType A.Region Name [Type]
  | TTypeQual A.Region Name Name [Type]
  | TRecord [(A.Located Name, Type)] (Maybe (A.Located Name))

-- MODULE

data Module = Module
  { _name :: Maybe (A.Located Name),
    _exports :: A.Located Exposing,
    _docs :: Docs,
    _imports :: [Import],
    _values :: [A.Located Value],
    _unions :: [A.Located Union],
    _aliases :: [A.Located Alias],
    _binops :: [A.Located Infix],
    _effects :: Effects
  }

getName :: Module -> Name
getName (Module maybeName _ _ _ _ _ _ _ _) =
  case maybeName of
    Just (A.At _ name) ->
      name
    Nothing ->
      Name._Main

getImportName :: Import -> Name
getImportName (Import (A.At _ name) _ _) =
  name

data Import = Import
  { _import :: A.Located Name,
    _alias :: Maybe Name,
    _exposing :: Exposing
  }

data Value = Value (A.Located Name) [Pattern] Expr (Maybe Type)

data Union = Union (A.Located Name) [A.Located Name] [(A.Located Name, [Type])]

data Alias = Alias (A.Located Name) [A.Located Name] Type

data Infix = Infix Name Binop.Associativity Binop.Precedence Name

data Port = Port (A.Located Name) Type

data Effects
  = NoEffects
  | Ports [Port]
  | Manager A.Region Manager

data Manager
  = Cmd (A.Located Name)
  | Sub (A.Located Name)
  | Fx (A.Located Name) (A.Located Name)

data Docs
  = NoDocs A.Region
  | YesDocs DocComment [(Name, DocComment)]

newtype DocComment
  = DocComment P.Snippet

-- EXPOSING

data Exposing
  = Open
  | Explicit [Exposed]

data Exposed
  = Lower (A.Located Name)
  | Upper (A.Located Name) Privacy
  | Operator A.Region Name

data Privacy
  = Public A.Region
  | Private
