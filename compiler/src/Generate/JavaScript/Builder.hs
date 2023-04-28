{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Generate.JavaScript.Builder
  ( Builder (..),
    emptyBuilder,
    stmtToBuilder,
    exprToBuilder,
    addByteString,
    Expr (..),
    LValue (..),
    Stmt (..),
    Case (..),
    InfixOp (..),
    PrefixOp (..),
  )
where

-- Based on the language-ecmascript package.
-- https://hackage.haskell.org/package/language-ecmascript
-- They did the hard work of reading the spec to figure out
-- how all the types should fit together.

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy.Char8 qualified as BSLazy
import Data.Function ((&))
import Generate.JavaScript.Name (Name)
import Generate.JavaScript.Name qualified as Name
import Json.Encode qualified as Json
import Prelude hiding (lines)

-- EXPRESSIONS

data Expr
  = String B.Builder
  | Float B.Builder
  | Int Int
  | Bool Bool
  | Null
  | Json Json.Value
  | Array [Expr]
  | Object [(Name, Expr)]
  | Ref Name
  | Access Expr Name -- foo.bar
  | Index Expr Expr -- foo[bar]
  | Prefix PrefixOp Expr
  | Infix InfixOp Expr Expr
  | If Expr Expr Expr
  | Assign LValue Expr
  | Call Expr [Expr]
  | Function (Maybe Name) [Name] [Stmt]

data LValue
  = LRef Name
  | LDot Expr Name
  | LBracket Expr Expr

-- STATEMENTS

data Stmt
  = Block [Stmt]
  | EmptyStmt
  | ExprStmt Expr
  | IfStmt Expr Stmt Stmt
  | Switch Expr [Case]
  | While Expr Stmt
  | Break (Maybe Name)
  | Continue (Maybe Name)
  | Labelled Name Stmt
  | Try Stmt Name Stmt
  | Throw Expr
  | Return Expr
  | Var Name Expr
  | Vars [(Name, Expr)]
  | FunctionStmt Name [Name] [Stmt]

data Case
  = Case Expr [Stmt]
  | Default [Stmt]

-- OPERATORS

data InfixOp
  = OpAdd -- +
  | OpSub -- -
  | OpMul
  | OpDiv -- /
  | OpMod -- %
  | OpEq -- ===
  | OpNe -- !==
  | OpLt -- <
  | OpLe -- <=
  | OpGt -- >
  | OpGe -- >=
  | OpAnd -- &&
  | OpOr
  | OpBitwiseAnd -- &
  | OpBitwiseXor
  | OpBitwiseOr
  | OpLShift -- <<
  | OpSpRShift -- >>
  | OpZfRShift -- >>>

data PrefixOp
  = PrefixNot -- !
  | PrefixNegate -- -
  | PrefixComplement -- ~

-- BUILDER

data Builder = Builder
  { _code :: B.Builder,
    _currentLine :: Int,
    _currentCol :: Int,
    _lines :: Lines,
    _mappings :: [Mapping]
  }

data Mapping = Mapping
  { _m_line :: Int,
    _m_col :: Int
  }

emptyBuilder :: Int -> Builder
emptyBuilder currentLine =
  Builder
    { _code = mempty,
      _currentLine = currentLine,
      _currentCol = 1,
      _lines = One,
      _mappings = []
    }

addAscii :: String -> Builder -> Builder
addAscii code (Builder _code _currLine _currCol _lines _mappings) =
  Builder
    { _code = _code <> B.string7 code,
      _currentLine = _currLine,
      _currentCol = _currCol + length code,
      _lines = _lines,
      _mappings = _mappings
    }

-- TODO: This is a crutch used during prototyping
-- Should be removed once things stabalizes as it's bad for perf
addByteString :: B.Builder -> Builder -> Builder
addByteString bsBuilder (Builder _code _currLine _currCol _lines _mappings) =
  let size = BSLazy.length $ B.toLazyByteString bsBuilder
   in Builder
        { _code = _code <> bsBuilder,
          _currentLine = _currLine,
          _currentCol = _currCol + fromIntegral size,
          _lines = _lines,
          _mappings = _mappings
        }

addLine :: Builder -> Builder
addLine (Builder _code _currLine _currCol _lines _mappings) =
  Builder
    { _code = _code <> B.char7 '\n',
      _currentLine = _currLine + 1,
      _currentCol = 1,
      _lines = Many,
      _mappings = _mappings
    }

-- ENCODE

stmtToBuilder :: Stmt -> Builder -> Builder
stmtToBuilder stmt builder =
  fromStmt levelZero stmt builder

exprToBuilder :: Expr -> Builder -> Builder
exprToBuilder expr builder =
  fromExpr levelZero Whatever expr builder

-- INDENT LEVEL

data Level
  = Level B.Builder Level

levelZero :: Level
levelZero =
  Level mempty (makeLevel 1 (BS.replicate 16 0x09 {-\t-}))

makeLevel :: Int -> BS.ByteString -> Level
makeLevel level oldTabs =
  let tabs =
        if level <= BS.length oldTabs
          then oldTabs
          else BS.replicate (BS.length oldTabs * 2) 0x09 {-\t-}
   in Level (B.byteString (BS.take level tabs)) (makeLevel (level + 1) tabs)

-- STATEMENTS

fromStmtBlock :: Level -> [Stmt] -> Builder -> Builder
fromStmtBlock level stmts builder =
  foldl (\accBuilder stmt -> fromStmt level stmt accBuilder) builder stmts

fromStmt :: Level -> Stmt -> Builder -> Builder
fromStmt level@(Level indent nextLevel) statement builder =
  case statement of
    Block stmts ->
      fromStmtBlock level stmts builder
    EmptyStmt ->
      builder
    ExprStmt expr ->
      builder
        & addByteString indent
        & fromExpr level Whatever expr
        & addAscii ";"
        & addLine
    IfStmt condition thenStmt elseStmt ->
      builder
        & addByteString indent
        & addAscii "if ("
        & fromExpr level Whatever condition
        & addAscii ") {"
        & addLine
        & fromStmt nextLevel thenStmt
        & addByteString indent
        & addAscii "} else {"
        & addLine
        & fromStmt nextLevel elseStmt
        & addAscii "}"
        & addLine
    Switch expr clauses ->
      builder
        & addByteString indent
        & addAscii "switch ("
        & fromExpr level Whatever expr
        & addAscii ") {"
        & addLine
        & fromClauses nextLevel clauses
        & addByteString indent
        & addAscii "}"
        & addLine
    While expr stmt ->
      builder
        & addByteString indent
        & addAscii "while ("
        & fromExpr level Whatever expr
        & addAscii ") {"
        & addLine
        & fromStmt nextLevel stmt
        & addByteString indent
        & addAscii "}"
        & addLine
    Break Nothing ->
      builder
        & addByteString indent
        & addAscii "break;"
        & addLine
    Break (Just label) ->
      builder
        & addByteString indent
        & addAscii "break "
        & addByteString (Name.toBuilder label)
        & addAscii ";"
        & addLine
    Continue Nothing ->
      builder
        & addByteString indent
        & addAscii "continue;"
        & addLine
    Continue (Just label) ->
      builder
        & addByteString indent
        & addAscii "continue "
        & addByteString (Name.toBuilder label)
        & addAscii ";"
        & addLine
    Labelled label stmt ->
      builder
        & addByteString indent
        & addByteString (Name.toBuilder label)
        & addAscii ":"
        & addLine
        & fromStmt level stmt
    Try tryStmt errorName catchStmt ->
      builder
        & addByteString indent
        & addAscii "try {"
        & addLine
        & fromStmt nextLevel tryStmt
        & addByteString indent
        & addAscii "} catch ("
        & addByteString (Name.toBuilder errorName)
        & addAscii ") {"
        & addLine
        & fromStmt nextLevel catchStmt
        & addByteString indent
        & addAscii "}"
        & addLine
    Throw expr ->
      builder
        & addByteString indent
        & addAscii "throw "
        & fromExpr level Whatever expr
        & addAscii ";"
    Return expr ->
      builder
        & addByteString indent
        & addAscii "return "
        & fromExpr level Whatever expr
        & addAscii ";"
        & addLine
    Var name expr ->
      builder
        & addByteString indent
        & addAscii "var "
        & addByteString (Name.toBuilder name)
        & addAscii " = "
        & fromExpr level Whatever expr
        & addAscii ";"
        & addLine
    Vars [] ->
      builder
    Vars vars ->
      builder
        & addByteString indent
        & addAscii "var "
        & commaNewlineSepExpr level (varToBuilder level) vars
        & addAscii ";"
        & addLine
    FunctionStmt name args stmts ->
      builder
        & addByteString indent
        & addAscii "function "
        & addByteString (Name.toBuilder name)
        & addAscii "("
        & commaSepExpr (addByteString . Name.toBuilder) args
        & addAscii ") {"
        & addLine
        & fromStmtBlock nextLevel stmts
        & addByteString indent
        & addAscii "}"
        & addLine

-- SWITCH CLAUSES

fromClause :: Level -> Case -> Builder -> Builder
fromClause level@(Level indent nextLevel) clause builder =
  case clause of
    Case expr stmts ->
      builder
        & addByteString indent
        & addAscii "case "
        & fromExpr level Whatever expr
        & addAscii ":"
        & addLine
        & fromStmtBlock nextLevel stmts
    Default stmts ->
      builder
        & addByteString indent
        & addAscii "default:"
        & addLine
        & fromStmtBlock nextLevel stmts

fromClauses :: Level -> [Case] -> Builder -> Builder
fromClauses level clauses builder =
  case clauses of
    [] ->
      builder
    first : rest ->
      fromClauses level rest (fromClause level first builder)

-- VAR DECLS

varToBuilder :: Level -> (Name, Expr) -> Builder -> Builder
varToBuilder level (name, expr) builder =
  builder
    & addByteString (Name.toBuilder name)
    & addAscii " = "
    & fromExpr level Whatever expr

-- EXPRESSIONS

data Lines = One | Many deriving (Eq)

commaSepExpr :: (a -> Builder -> Builder) -> [a] -> Builder -> Builder
commaSepExpr fn exprs builder =
  case exprs of
    [] ->
      builder
    [first] ->
      fn first builder
    first : rest ->
      commaSepExpr fn rest (addAscii ", " (fn first builder))

commaNewlineSepExpr :: Level -> (a -> Builder -> Builder) -> [a] -> Builder -> Builder
commaNewlineSepExpr level@(Level indent _) fn exprs builder =
  case exprs of
    [] ->
      builder
    [first] ->
      fn first builder
    first : rest ->
      commaNewlineSepExpr level fn rest (addByteString indent (addLine (addAscii "," (fn first builder))))

data Grouping = Atomic | Whatever

parensFor :: Grouping -> Builder -> (Builder -> Builder) -> Builder
parensFor grouping builder fillContent =
  case grouping of
    Atomic ->
      builder
        & addAscii "("
        & fillContent
        & addAscii ")"
    Whatever ->
      fillContent builder

fromExpr :: Level -> Grouping -> Expr -> Builder -> Builder
fromExpr level@(Level indent nextLevel) grouping expression builder =
  case expression of
    String string ->
      addByteString ("''" <> string <> "''") builder
    Float float ->
      addByteString float builder
    Int n ->
      addByteString (B.intDec n) builder
    Bool bool ->
      addAscii (if bool then "true" else "false") builder
    Null ->
      addAscii "null" builder
    Json json ->
      addByteString (Json.encodeUgly json) builder
    Array exprs ->
      builder
        & addAscii "[ "
        & commaSepExpr (fromExpr level Whatever) exprs
        & addAscii " ]"
    Object fields ->
      builder
        & addAscii "{ "
        & commaSepExpr (fromField level) fields
        & addAscii " }"
    Ref name ->
      addByteString (Name.toBuilder name) builder
    Access expr field ->
      makeDot level expr field builder
    Index expr bracketedExpr ->
      makeBracketed level expr bracketedExpr builder
    Prefix op expr ->
      parensFor grouping builder $ \b ->
        b
          & fromPrefix op
          & fromExpr level Atomic expr
    Infix op leftExpr rightExpr ->
      parensFor grouping builder $ \b ->
        b
          & fromExpr level Atomic leftExpr
          & fromInfix op
          & fromExpr level Atomic rightExpr
    If condExpr thenExpr elseExpr ->
      parensFor grouping builder $ \b ->
        b
          & fromExpr level Atomic condExpr
          & addAscii " ? "
          & fromExpr level Atomic thenExpr
          & addAscii " : "
          & fromExpr level Atomic elseExpr
    Assign lValue expr ->
      parensFor grouping builder $ \b ->
        b
          & fromLValue level lValue
          & addAscii " = "
          & fromExpr level Whatever expr
    Call function args ->
      builder
        & fromExpr level Atomic function
        & addAscii "("
        & commaSepExpr (fromExpr nextLevel Whatever) args
        & addAscii ")"
    Function maybeName args stmts ->
      builder
        & addAscii "function "
        & addByteString (maybe mempty Name.toBuilder maybeName)
        & addAscii "("
        & commaSepExpr (addByteString . Name.toBuilder) args
        & addAscii ") {"
        & addLine
        & fromStmtBlock nextLevel stmts
        & addByteString indent
        & addAscii "}"

-- FIELDS

fromField :: Level -> (Name, Expr) -> Builder -> Builder
fromField level (field, expr) builder =
  builder
    & addByteString (Name.toBuilder field)
    & addAscii ": "
    & fromExpr level Whatever expr

-- VALUES

fromLValue :: Level -> LValue -> Builder -> Builder
fromLValue level lValue builder =
  case lValue of
    LRef name ->
      addByteString (Name.toBuilder name) builder
    LDot expr field ->
      makeDot level expr field builder
    LBracket expr bracketedExpr ->
      makeBracketed level expr bracketedExpr builder

makeDot :: Level -> Expr -> Name -> Builder -> Builder
makeDot level expr field builder =
  builder
    & fromExpr level Atomic expr
    & addAscii "."
    & addByteString (Name.toBuilder field)

makeBracketed :: Level -> Expr -> Expr -> Builder -> Builder
makeBracketed level expr bracketedExpr builder =
  builder
    & fromExpr level Atomic expr
    & addAscii "["
    & fromExpr level Whatever bracketedExpr
    & addAscii "]"

-- OPERATORS

fromPrefix :: PrefixOp -> Builder -> Builder
fromPrefix op =
  addAscii $
    case op of
      PrefixNot -> "!"
      PrefixNegate -> "-"
      PrefixComplement -> "~"

fromInfix :: InfixOp -> Builder -> Builder
fromInfix op =
  addAscii $
    case op of
      OpAdd -> " + "
      OpSub -> " - "
      OpMul -> " * "
      OpDiv -> " / "
      OpMod -> " % "
      OpEq -> " === "
      OpNe -> " !== "
      OpLt -> " < "
      OpLe -> " <= "
      OpGt -> " > "
      OpGe -> " >= "
      OpAnd -> " && "
      OpOr -> " || "
      OpBitwiseAnd -> " & "
      OpBitwiseXor -> " ^ "
      OpBitwiseOr -> " | "
      OpLShift -> " << "
      OpSpRShift -> " >> "
      OpZfRShift -> " >>> "
