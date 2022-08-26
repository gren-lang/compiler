{-# LANGUAGE OverloadedStrings #-}
-- Temporary while implementing gren format
{-# OPTIONS_GHC -Wno-error=unused-do-bind #-}

module Parse.Declaration
  ( Decl (..),
    declaration,
    infix_,
  )
where

import AST.Source qualified as Src
import AST.Utils.Binop qualified as Binop
import Data.Name qualified as Name
import Parse.Expression qualified as Expr
import Parse.Keyword qualified as Keyword
import Parse.Number qualified as Number
import Parse.Pattern qualified as Pattern
import Parse.Primitives hiding (State)
import Parse.Primitives qualified as P
import Parse.Space qualified as Space
import Parse.Symbol qualified as Symbol
import Parse.Type qualified as Type
import Parse.Variable qualified as Var
import Reporting.Annotation qualified as A
import Reporting.Error.Syntax qualified as E

-- DECLARATION

data Decl
  = Value (Maybe Src.DocComment) (A.Located Src.Value)
  | Union (Maybe Src.DocComment) (A.Located Src.Union)
  | Alias (Maybe Src.DocComment) (A.Located Src.Alias)
  | Port (Maybe Src.DocComment) Src.Port

declaration :: Space.Parser E.Decl Decl
declaration =
  do
    maybeDocs <- chompDocComment
    start <- getPosition
    oneOf
      E.DeclStart
      [ typeDecl maybeDocs start,
        portDecl maybeDocs,
        valueDecl maybeDocs start
      ]

-- DOC COMMENT

chompDocComment :: Parser E.Decl (Maybe Src.DocComment)
chompDocComment =
  oneOfWithFallback
    [ do
        docComment <- Space.docComment E.DeclStart E.DeclSpace
        _ <- Space.chomp E.DeclSpace
        Space.checkFreshLine E.DeclFreshLineAfterDocComment
        return (Just docComment)
    ]
    Nothing

-- DEFINITION and ANNOTATION

valueDecl :: Maybe Src.DocComment -> A.Position -> Space.Parser E.Decl Decl
valueDecl maybeDocs start =
  do
    name <- Var.lower E.DeclStart
    end <- getPosition
    specialize (E.DeclDef name) $
      do
        _ <- Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
        oneOf
          E.DeclDefEquals
          [ do
              word1 0x3A {-:-} E.DeclDefEquals
              _ <- Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentType
              (tipe, _) <- specialize E.DeclDefType Type.expression
              Space.checkFreshLine E.DeclDefNameRepeat
              defName <- chompMatchingName name
              _ <- Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
              chompDefArgsAndBody maybeDocs start defName (Just tipe) [],
            chompDefArgsAndBody maybeDocs start (A.at start end name) Nothing []
          ]

chompDefArgsAndBody :: Maybe Src.DocComment -> A.Position -> A.Located Name.Name -> Maybe Src.Type -> [Src.Pattern] -> Space.Parser E.DeclDef Decl
chompDefArgsAndBody maybeDocs start name tipe revArgs =
  oneOf
    E.DeclDefEquals
    [ do
        arg <- specialize E.DeclDefArg Pattern.term
        _ <- Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
        chompDefArgsAndBody maybeDocs start name tipe (arg : revArgs),
      do
        word1 0x3D {-=-} E.DeclDefEquals
        _ <- Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentBody
        (body, end) <- specialize E.DeclDefBody Expr.expression
        let value = Src.Value name (reverse revArgs) body tipe
        let avalue = A.at start end value
        return (Value maybeDocs avalue, end)
    ]

chompMatchingName :: Name.Name -> Parser E.DeclDef (A.Located Name.Name)
chompMatchingName expectedName =
  let (P.Parser parserL) = Var.lower E.DeclDefNameRepeat
   in P.Parser $ \state@(P.State _ _ _ _ sr sc) cok eok cerr eerr ->
        let cokL name newState@(P.State _ _ _ _ er ec) =
              if expectedName == name
                then cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState
                else cerr sr sc (E.DeclDefNameMatch name)

            eokL name newState@(P.State _ _ _ _ er ec) =
              if expectedName == name
                then eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState
                else eerr sr sc (E.DeclDefNameMatch name)
         in parserL state cokL eokL cerr eerr

-- TYPE DECLARATIONS

typeDecl :: Maybe Src.DocComment -> A.Position -> Space.Parser E.Decl Decl
typeDecl maybeDocs start =
  inContext E.DeclType (Keyword.type_ E.DeclStart) $
    do
      _ <- Space.chompAndCheckIndent E.DT_Space E.DT_IndentName
      oneOf
        E.DT_Name
        [ inContext E.DT_Alias (Keyword.alias_ E.DT_Name) $
            do
              _ <- Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
              (name, args) <- chompAliasNameToEquals
              (tipe, end) <- specialize E.AliasBody Type.expression
              let alias = A.at start end (Src.Alias name args tipe)
              return (Alias maybeDocs alias, end),
          specialize E.DT_Union $
            do
              (name, args) <- chompCustomNameToEquals
              (firstVariant, firstEnd) <- Type.variant
              (variants, end) <- chompVariants [firstVariant] firstEnd
              let union = A.at start end (Src.Union name args variants)
              return (Union maybeDocs union, end)
        ]

-- TYPE ALIASES

chompAliasNameToEquals :: Parser E.TypeAlias (A.Located Name.Name, [A.Located Name.Name])
chompAliasNameToEquals =
  do
    name <- addLocation (Var.upper E.AliasName)
    _ <- Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
    chompAliasNameToEqualsHelp name []

chompAliasNameToEqualsHelp :: A.Located Name.Name -> [A.Located Name.Name] -> Parser E.TypeAlias (A.Located Name.Name, [A.Located Name.Name])
chompAliasNameToEqualsHelp name args =
  oneOf
    E.AliasEquals
    [ do
        arg <- addLocation (Var.lower E.AliasEquals)
        _ <- Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
        chompAliasNameToEqualsHelp name (arg : args),
      do
        word1 0x3D {-=-} E.AliasEquals
        _ <- Space.chompAndCheckIndent E.AliasSpace E.AliasIndentBody
        return (name, reverse args)
    ]

-- CUSTOM TYPES

chompCustomNameToEquals :: Parser E.CustomType (A.Located Name.Name, [A.Located Name.Name])
chompCustomNameToEquals =
  do
    name <- addLocation (Var.upper E.CT_Name)
    _ <- Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
    chompCustomNameToEqualsHelp name []

chompCustomNameToEqualsHelp :: A.Located Name.Name -> [A.Located Name.Name] -> Parser E.CustomType (A.Located Name.Name, [A.Located Name.Name])
chompCustomNameToEqualsHelp name args =
  oneOf
    E.CT_Equals
    [ do
        arg <- addLocation (Var.lower E.CT_Equals)
        _ <- Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
        chompCustomNameToEqualsHelp name (arg : args),
      do
        word1 0x3D {-=-} E.CT_Equals
        _ <- Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterEquals
        return (name, reverse args)
    ]

chompVariants :: [(A.Located Name.Name, [Src.Type])] -> A.Position -> Space.Parser E.CustomType [(A.Located Name.Name, [Src.Type])]
chompVariants variants end =
  oneOfWithFallback
    [ do
        Space.checkIndent end E.CT_IndentBar
        word1 0x7C E.CT_Bar
        _ <- Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterBar
        (variant, newEnd) <- Type.variant
        chompVariants (variant : variants) newEnd
    ]
    (reverse variants, end)

-- PORT

portDecl :: Maybe Src.DocComment -> Space.Parser E.Decl Decl
portDecl maybeDocs =
  inContext E.Port (Keyword.port_ E.DeclStart) $
    do
      _ <- Space.chompAndCheckIndent E.PortSpace E.PortIndentName
      name <- addLocation (Var.lower E.PortName)
      _ <- Space.chompAndCheckIndent E.PortSpace E.PortIndentColon
      word1 0x3A {-:-} E.PortColon
      _ <- Space.chompAndCheckIndent E.PortSpace E.PortIndentType
      (tipe, end) <- specialize E.PortType Type.expression
      return
        ( Port maybeDocs (Src.Port name tipe),
          end
        )

-- INFIX

-- INVARIANT: always chomps to a freshline
--
infix_ :: Parser E.Module (A.Located Src.Infix)
infix_ =
  let err = E.Infix
      _err = \_ -> E.Infix
   in do
        start <- getPosition
        Keyword.infix_ err
        _ <- Space.chompAndCheckIndent _err err
        associativity <-
          oneOf
            err
            [ Keyword.left_ err >> return Binop.Left,
              Keyword.right_ err >> return Binop.Right,
              Keyword.non_ err >> return Binop.Non
            ]
        _ <- Space.chompAndCheckIndent _err err
        precedence <- Number.precedence err
        _ <- Space.chompAndCheckIndent _err err
        word1 0x28 {-(-} err
        op <- Symbol.operator err _err
        word1 0x29 {-)-} err
        _ <- Space.chompAndCheckIndent _err err
        word1 0x3D {-=-} err
        _ <- Space.chompAndCheckIndent _err err
        name <- Var.lower err
        end <- getPosition
        _ <- Space.chomp _err
        Space.checkFreshLine err
        return (A.at start end (Src.Infix op associativity precedence name))
