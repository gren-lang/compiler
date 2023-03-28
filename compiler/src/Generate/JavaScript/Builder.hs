{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Generate.JavaScript.Builder
  ( stmtToBuilder,
    exprToBuilder,
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
import Data.List qualified as List
import Generate.JavaScript.Name (Name)
import Generate.JavaScript.Name qualified as Name
import Json.Encode qualified as Json
import Prelude hiding (lines)

-- EXPRESSIONS

-- NOTE: I tried making this create a B.Builder directly.
--
-- The hope was that it'd allocate less and speed things up, but it seemed
-- to be neutral for perf.
--
-- The downside is that Generate.JavaScript.Expression inspects the
-- structure of Expr and Stmt on some occassions to try to strip out
-- unnecessary closures. I think these closures are already avoided
-- by other logic in code gen these days, but I am not 100% certain.
--
-- For this to be worth it, I think it would be necessary to avoid
-- returning tuples when generating expressions.
--
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

stmtToBuilder :: Int -> Stmt -> Builder
stmtToBuilder line stmts =
  fromStmt line levelZero stmts

exprToBuilder :: Int -> Expr -> Builder
exprToBuilder line expr =
  snd $ fromExpr line levelZero Whatever expr

-- INDENT LEVEL

data Level
  = Level Builder Level

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

-- HELPERS

commaSep :: [Builder] -> Builder
commaSep builders =
  mconcat (List.intersperse ", " builders)

commaNewlineSep :: Level -> [Builder] -> Builder
commaNewlineSep (Level _ (Level deeperIndent _)) builders =
  mconcat (List.intersperse (",\n" <> deeperIndent) builders)

-- STATEMENTS

fromStmtBlock :: Int -> Level -> [Stmt] -> Builder
fromStmtBlock line level stmts =
  mconcat (map (fromStmt line level) stmts)

fromStmt :: Int -> Level -> Stmt -> Builder
fromStmt line level@(Level indent nextLevel) statement =
  case statement of
    Block stmts ->
      fromStmtBlock line level stmts
    EmptyStmt ->
      mempty
    ExprStmt expr ->
      indent <> snd (fromExpr line level Whatever expr) <> ";\n"
    IfStmt condition thenStmt elseStmt ->
      mconcat
        [ indent,
          "if (",
          snd (fromExpr line level Whatever condition),
          ") {\n",
          fromStmt (line + 1) nextLevel thenStmt,
          indent,
          "} else {\n",
          fromStmt (line + 2) nextLevel elseStmt,
          indent,
          "}\n"
        ]
    Switch expr clauses ->
      mconcat
        [ indent,
          "switch (",
          snd (fromExpr line level Whatever expr),
          ") {\n",
          mconcat (map (fromClause (line + 1) nextLevel) clauses),
          indent,
          "}\n"
        ]
    While expr stmt ->
      mconcat
        [ indent,
          "while (",
          snd (fromExpr line level Whatever expr),
          ") {\n",
          fromStmt (line + 1) nextLevel stmt,
          indent,
          "}\n"
        ]
    Break Nothing ->
      indent <> "break;\n"
    Break (Just label) ->
      indent <> "break " <> Name.toBuilder label <> ";\n"
    Continue Nothing ->
      indent <> "continue;\n"
    Continue (Just label) ->
      indent <> "continue " <> Name.toBuilder label <> ";\n"
    Labelled label stmt ->
      mconcat
        [ indent,
          Name.toBuilder label,
          ":\n",
          fromStmt (line + 1) level stmt
        ]
    Try tryStmt errorName catchStmt ->
      mconcat
        [ indent,
          "try {\n",
          fromStmt (line + 1) nextLevel tryStmt,
          indent,
          "} catch (",
          Name.toBuilder errorName,
          ") {\n",
          fromStmt line nextLevel catchStmt,
          indent,
          "}\n"
        ]
    Throw expr ->
      indent <> "throw " <> snd (fromExpr line level Whatever expr) <> ";"
    Return expr ->
      indent <> "return " <> snd (fromExpr line level Whatever expr) <> ";\n"
    Var name expr ->
      indent <> "var " <> Name.toBuilder name <> " = " <> snd (fromExpr line level Whatever expr) <> ";\n"
    Vars [] ->
      mempty
    Vars vars ->
      indent <> "var " <> commaNewlineSep level (map (varToBuilder line level) vars) <> ";\n"
    FunctionStmt name args stmts ->
      indent
        <> "function "
        <> Name.toBuilder name
        <> "("
        <> commaSep (map Name.toBuilder args)
        <> ") {\n"
        <> fromStmtBlock (line + 1) nextLevel stmts
        <> indent
        <> "}\n"

-- SWITCH CLAUSES

fromClause :: Int -> Level -> Case -> Builder
fromClause line level@(Level indent nextLevel) clause =
  case clause of
    Case expr stmts ->
      indent
        <> "case "
        <> snd (fromExpr line level Whatever expr)
        <> ":\n"
        <> fromStmtBlock (line + 1) nextLevel stmts
    Default stmts ->
      indent
        <> "default:\n"
        <> fromStmtBlock (line + 1) nextLevel stmts

-- VAR DECLS

varToBuilder :: Int -> Level -> (Name, Expr) -> Builder
varToBuilder line level (name, expr) =
  Name.toBuilder name <> " = " <> snd (fromExpr line level Whatever expr)

-- EXPRESSIONS

data Lines = One | Many deriving (Eq)

merge :: Lines -> Lines -> Lines
merge a b =
  if a == Many || b == Many then Many else One

linesMap :: (a -> (Lines, b)) -> [a] -> (Bool, [b])
linesMap func xs =
  let pairs = map func xs
   in ( any ((==) Many . fst) pairs,
        map snd pairs
      )

commaSepExpr :: (a -> Builder -> Builder) -> [a] -> Builder -> Builder
commaSepExpr fn exprs builder =
  case exprs of
    [] ->
      builder
    [first] ->
      fn first builder
    first : rest ->
      commaSepExpr fn rest (addAscii ", " (fn first builder))

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

fromExpr :: Int -> Level -> Grouping -> Expr -> Builder -> Builder
fromExpr line level@(Level indent nextLevel@(Level deeperIndent _)) grouping expression builder =
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
        & commaSepExpr (fromExpr line level Whatever) exprs
        & addAscii " ]"
    Object fields ->
      builder
        & addAscii "{ "
        & commaSepExpr (fromField line level) fields
        & addAscii " }"
    Ref name ->
      addByteString (Name.toBuilder name) builder
    Access expr field ->
      makeDot line level expr field builder
    Index expr bracketedExpr ->
      makeBracketed line level expr bracketedExpr builder
    Prefix op expr ->
      parensFor grouping builder $ \b ->
        b
          & fromPrefix op
          & fromExpr line level Atomic expr
    Infix op leftExpr rightExpr ->
      parensFor grouping builder $ \b ->
        b
          & fromExpr line level Atomic leftExpr
          & fromInfix op
          & fromExpr line level Atomic rightExpr
    If condExpr thenExpr elseExpr ->
      parensFor grouping builder $ \b ->
        b
          & fromExpr line level Atomic condExpr
          & addAscii " ? "
          & fromExpr line level Atomic thenExpr
          & addAscii " : "
          & fromExpr line level Atomic elseExpr
    Assign lValue expr ->
      parensFor grouping builder $ \b ->
        b
          & fromLValue line level lValue
          & addAscii " = "
          & fromExpr line level Whatever expr
    Call function args ->
      builder
        & fromExpr line level Atomic function
        & addAscii "("
        & commaSepExpr (fromExpr line nextLevel Whatever) args
        & addAscii ")"
    Function maybeName args stmts ->
      builder
        & addAscii "function "
        & addByteString (maybe mempty Name.toBuilder maybeName)
        & addAscii "("
        & commaSepExpr (\arg -> addByteString (Name.toBuilder arg)) args
        & addAscii ") {"
        & addLine
        & fromStmtBlock (line + 1) nextLevel stmts
        & addByteString indent
        & addAscii "}"

-- FIELDS

fromField :: Int -> Level -> (Name, Expr) -> Builder -> Builder
fromField line level (field, expr) builder =
  builder
    & addByteString (Name.toBuilder field)
    & addAscii ": "
    & fromExpr line level Whatever expr

-- VALUES

fromLValue :: Int -> Level -> LValue -> Builder -> Builder
fromLValue line level lValue builder =
  case lValue of
    LRef name ->
      addByteString (Name.toBuilder name) builder
    LDot expr field ->
      makeDot line level expr field builder
    LBracket expr bracketedExpr ->
      makeBracketed line level expr bracketedExpr builder

makeDot :: Int -> Level -> Expr -> Name -> Builder -> Builder
makeDot line level expr field builder =
  builder
    & fromExpr line level Atomic expr
    & addAscii "."
    & addByteString (Name.toBuilder field)

makeBracketed :: Int -> Level -> Expr -> Expr -> Builder -> Builder
makeBracketed line level expr bracketedExpr builder =
  builder
    & fromExpr line level Atomic expr
    & addAscii "["
    & fromExpr line level Whatever bracketedExpr
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
