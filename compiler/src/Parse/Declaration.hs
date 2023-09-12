{-# LANGUAGE OverloadedStrings #-}
-- Temporary while implementing gren format
{-# OPTIONS_GHC -Wno-error=unused-do-bind #-}
{-# OPTIONS_GHC -Wno-error=unused-local-binds #-}
{-# OPTIONS_GHC -Wno-error=unused-matches #-}

module Parse.Declaration
  ( Decl (..),
    declaration,
    infix_,
  )
where

import AST.Source qualified as Src
import AST.SourceComments qualified as SC
import AST.Utils.Binop qualified as Binop
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
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
  | AliasConstraint (Maybe Src.DocComment) (A.Located Src.AliasConstraint)
  | ValueConstraint (Maybe Src.DocComment) (A.Located Src.ValueConstraint)
  | Port (Maybe Src.DocComment) Src.Port
  | TopLevelComments (NonEmpty Src.Comment)
  deriving (Show)

declaration :: Space.Parser E.Decl (Decl, [Src.Comment])
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
        Space.chomp E.DeclSpace
        Space.checkFreshLine E.DeclFreshLineAfterDocComment
        return (Just docComment)
    ]
    Nothing

-- DEFINITION and ANNOTATION

valueDecl :: Maybe Src.DocComment -> A.Position -> Space.Parser E.Decl (Decl, [Src.Comment])
valueDecl maybeDocs start =
  do
    name <- Var.lower E.DeclStart
    end <- getPosition
    specialize (E.DeclDef name) $
      do
        commentsAfterName <- Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
        oneOf
          E.DeclDefEquals
          [ do
              word1 0x3A {-:-} E.DeclDefEquals
              let commentsBeforeColon = commentsAfterName
              commentsAfterColon <- Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentType
              ((tipe, commentsAfterTipe), _) <- specialize E.DeclDefType Type.expression
              Space.checkFreshLine E.DeclDefNameRepeat
              oneOf
                E.DeclDefEquals
                [ do
                    defName <- chompMatchingName name
                    commentsAfterMatchingName <- Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                    let tipeComments = SC.ValueTypeComments commentsBeforeColon commentsAfterColon commentsAfterTipe
                    chompDefArgsAndBody maybeDocs start defName (Just (tipe, tipeComments)) [] commentsAfterMatchingName,
                  do
                    typeEnd <- getPosition
                    let value = Src.ValueConstraint (A.at start end name) tipe
                    return ((ValueConstraint maybeDocs (A.at start typeEnd value), commentsAfterTipe), typeEnd)
                ],
            chompDefArgsAndBody maybeDocs start (A.at start end name) Nothing [] commentsAfterName
          ]

chompDefArgsAndBody :: Maybe Src.DocComment -> A.Position -> A.Located Name.Name -> Maybe (Src.Type, SC.ValueTypeComments) -> [([Src.Comment], Src.Pattern)] -> [Src.Comment] -> Space.Parser E.DeclDef (Decl, [Src.Comment])
chompDefArgsAndBody maybeDocs start name tipe revArgs commentsBefore =
  oneOf
    E.DeclDefEquals
    [ do
        arg <- specialize E.DeclDefArg Pattern.term
        commentsAfterArg <- Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
        chompDefArgsAndBody maybeDocs start name tipe ((commentsBefore, arg) : revArgs) commentsAfterArg,
      do
        word1 0x3D {-=-} E.DeclDefEquals
        commentsAfterEquals <- Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentBody
        ((body, commentsAfter), end) <- specialize E.DeclDefBody Expr.expression
        let (commentsAfterBody, commentsAfterDef) = List.span (A.isIndentedMoreThan 1) commentsAfter
        let comments = SC.ValueComments commentsBefore commentsAfterEquals commentsAfterBody
        let value = Src.Value name (reverse revArgs) body tipe comments
        let avalue = A.at start end value
        return ((Value maybeDocs avalue, commentsAfterDef), end)
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

typeDecl :: Maybe Src.DocComment -> A.Position -> Space.Parser E.Decl (Decl, [Src.Comment])
typeDecl maybeDocs start =
  inContext E.DeclType (Keyword.type_ E.DeclStart) $
    do
      commentsAfterTypeKeyword <- Space.chompAndCheckIndent E.DT_Space E.DT_IndentName
      oneOf
        E.DT_Name
        [ inContext E.DT_Alias (Keyword.alias_ E.DT_Name) $
            do
              -- TODO: use commentsAfterTypeKeyword
              Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
              name <- addLocation (Var.upper E.AliasName)
              Space.chomp E.AliasSpace
              oneOf
                E.AliasEquals
                [ do
                    currentPos <- getPosition
                    Space.checkIndent currentPos E.AliasIndentEquals
                    args <- chompAliasArgsToEquals []
                    ((tipe, commentsAfterTipe), end) <- specialize E.AliasBody Type.expression
                    let alias = A.at start end (Src.Alias name args tipe)
                    return ((Alias maybeDocs alias, commentsAfterTipe), end),
                  do
                    end <- getPosition
                    return ((AliasConstraint maybeDocs (A.at start end (Src.AliasConstraint name)), []), end)
                ],
          specialize E.DT_Union $
            do
              (name, args, commentsAfterArgs, commentsAfterEquals) <- chompCustomNameToEquals
              ((firstName, firstArgs, commentsAfterFirst), firstEnd) <- Type.variant
              let firstVariant = (commentsAfterEquals, firstName, firstArgs, commentsAfterFirst)
              ((variants, commentsAfter), end) <- chompVariants (NonEmpty.singleton firstVariant) firstEnd
              let comments = SC.UnionComments commentsAfterTypeKeyword commentsAfterArgs
              let union = A.at start end (Src.Union name args variants comments)
              return ((Union maybeDocs union, commentsAfter), end)
        ]

-- TYPE ALIASES

chompAliasArgsToEquals :: [A.Located Name.Name] -> Parser E.TypeAlias [A.Located Name.Name]
chompAliasArgsToEquals args =
  oneOf
    E.AliasEquals
    [ do
        arg <- addLocation (Var.lower E.AliasEquals)
        Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
        chompAliasArgsToEquals (arg : args),
      do
        word1 0x3D {-=-} E.AliasEquals
        Space.chompAndCheckIndent E.AliasSpace E.AliasIndentBody
        return (reverse args)
    ]

-- CUSTOM TYPES

chompCustomNameToEquals :: Parser E.CustomType (A.Located Name.Name, [([Src.Comment], A.Located Name.Name)], [Src.Comment], [Src.Comment])
chompCustomNameToEquals =
  do
    name <- addLocation (Var.upper E.CT_Name)
    commentsAfterName <- Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
    chompCustomNameToEqualsHelp name commentsAfterName []

chompCustomNameToEqualsHelp :: A.Located Name.Name -> [Src.Comment] -> [([Src.Comment], A.Located Name.Name)] -> Parser E.CustomType (A.Located Name.Name, [([Src.Comment], A.Located Name.Name)], [Src.Comment], [Src.Comment])
chompCustomNameToEqualsHelp name commentsAfterPrev args =
  oneOf
    E.CT_Equals
    [ do
        arg <- addLocation (Var.lower E.CT_Equals)
        commentsAfterArg <- Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
        chompCustomNameToEqualsHelp name commentsAfterArg ((commentsAfterPrev, arg) : args),
      do
        word1 0x3D {-=-} E.CT_Equals
        commentsAfter <- Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterEquals
        return (name, reverse args, commentsAfterPrev, commentsAfter)
    ]

chompVariants :: NonEmpty (Src.UnionVariant) -> A.Position -> Space.Parser E.CustomType ([Src.UnionVariant], [Src.Comment])
chompVariants variants@((lastCommentsBefore, lastName, lastArgs, lastCommentsAfter) :| rest) end =
  oneOfWithFallback
    [ do
        Space.checkIndent end E.CT_IndentBar
        word1 0x7C E.CT_Bar
        commentsAfterBar <- Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterBar
        ((name, args, commentsAfter), newEnd) <- Type.variant
        let variant = (commentsAfterBar, name, args, commentsAfter)
        chompVariants (NonEmpty.cons variant variants) newEnd
    ]
    ( let (commentsAfterLastVariant, commentsAfter) = List.span (A.isIndentedMoreThan 1) lastCommentsAfter
       in ((reverse ((lastCommentsBefore, lastName, lastArgs, commentsAfterLastVariant) : rest), commentsAfter), end)
    )

-- PORT

portDecl :: Maybe Src.DocComment -> Space.Parser E.Decl (Decl, [Src.Comment])
portDecl maybeDocs =
  inContext E.Port (Keyword.port_ E.DeclStart) $
    do
      Space.chompAndCheckIndent E.PortSpace E.PortIndentName
      name <- addLocation (Var.lower E.PortName)
      Space.chompAndCheckIndent E.PortSpace E.PortIndentColon
      word1 0x3A {-:-} E.PortColon
      Space.chompAndCheckIndent E.PortSpace E.PortIndentType
      ((tipe, commentsAfterTipe), end) <- specialize E.PortType Type.expression
      return
        ( (Port maybeDocs (Src.Port name tipe), commentsAfterTipe),
          end
        )

-- INFIX

-- INVARIANT: always chomps to a freshline
--
infix_ :: Parser E.Module (A.Located Src.Infix, [Src.Comment])
infix_ =
  let err = E.Infix
      _err = \_ -> E.Infix
   in do
        start <- getPosition
        Keyword.infix_ err
        Space.chompAndCheckIndent _err err
        associativity <-
          oneOf
            err
            [ Keyword.left_ err >> return Binop.Left,
              Keyword.right_ err >> return Binop.Right,
              Keyword.non_ err >> return Binop.Non
            ]
        Space.chompAndCheckIndent _err err
        precedence <- Number.precedence err
        Space.chompAndCheckIndent _err err
        word1 0x28 {-(-} err
        op <- Symbol.operator err _err
        word1 0x29 {-)-} err
        Space.chompAndCheckIndent _err err
        word1 0x3D {-=-} err
        Space.chompAndCheckIndent _err err
        name <- Var.lower err
        end <- getPosition
        commentsAfter <- Space.chomp _err
        Space.checkFreshLine err
        return (A.at start end (Src.Infix op associativity precedence name), commentsAfter)
