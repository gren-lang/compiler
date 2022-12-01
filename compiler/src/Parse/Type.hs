{-# LANGUAGE OverloadedStrings #-}
-- Temporary while implementing gren format
{-# OPTIONS_GHC -Wno-error=unused-do-bind #-}
{-# OPTIONS_GHC -Wno-error=unused-matches #-}

module Parse.Type
  ( expression,
    variant,
  )
where

import AST.Source qualified as Src
import AST.SourceComments qualified as SC
import Data.Name qualified as Name
import Parse.Primitives (Parser, addEnd, addLocation, getPosition, inContext, oneOf, oneOfWithFallback, specialize, word1, word2)
import Parse.Space qualified as Space
import Parse.Variable qualified as Var
import Reporting.Annotation qualified as A
import Reporting.Error.Syntax qualified as E

-- TYPE TERMS

term :: Parser E.Type Src.Type
term =
  do
    start <- getPosition
    oneOf
      E.TStart
      [ -- types with no arguments (Int, Float, etc.)
        do
          upper <- Var.foreignUpper E.TStart
          end <- getPosition
          let region = A.Region start end
          return $
            A.At region $
              case upper of
                Var.Unqualified name ->
                  Src.TType region name []
                Var.Qualified home name ->
                  Src.TTypeQual region home name [],
        -- type variables
        do
          var <- Var.lower E.TStart
          addEnd start (Src.TVar var),
        -- parenthesis
        inContext E.TParenthesis (word1 0x28 {-(-} E.TStart) $
          do
            commentsAfterOpeningParen <- Space.chompAndCheckIndent E.TParenthesisSpace E.TParenthesisIndentOpen
            ((tipe, commentsBeforeClosingParen), end) <- specialize E.TParenthesisType expression
            Space.checkIndent end E.TParenthesisIndentEnd
            word1 0x29 {-)-} E.TParenthesisEnd
            let comments = SC.TParensComments commentsAfterOpeningParen commentsBeforeClosingParen
            addEnd start (Src.TParens tipe comments),
        -- records
        inContext E.TRecord (word1 0x7B {- { -} E.TStart) $
          do
            commentsAfterOpenBrace <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentOpen
            oneOf
              E.TRecordOpen
              [ do
                  word1 0x7D {-}-} E.TRecordEnd
                  addEnd start (Src.TRecord [] Nothing),
                do
                  name <- addLocation (Var.lower E.TRecordField)
                  commentsAfterFirstName <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon
                  oneOf
                    E.TRecordColon
                    [ do
                        word1 0x7C E.TRecordColon
                        commentsAfterBar <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField
                        let baseComments = SC.UpdateComments commentsAfterOpenBrace commentsAfterFirstName
                        field <- chompField commentsAfterBar
                        fields <- chompRecordEnd [field]
                        addEnd start (Src.TRecord fields (Just (name, baseComments))),
                      do
                        word1 0x3A {-:-} E.TRecordColon
                        commentsAfterColon <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType
                        ((tipe, commentsAfterTipe), end) <- specialize E.TRecordType expression
                        Space.checkIndent end E.TRecordIndentEnd
                        let fieldComments = SC.RecordFieldComments commentsAfterOpenBrace commentsAfterFirstName commentsAfterColon commentsAfterTipe
                        let field = (name, tipe, fieldComments)
                        fields <- chompRecordEnd [field]
                        addEnd start (Src.TRecord fields Nothing)
                    ]
              ]
      ]

-- TYPE EXPRESSIONS

expression :: Space.Parser E.Type (Src.Type, [Src.Comment])
expression =
  do
    start <- getPosition
    term1@((tipe1, commentsBeforeArrow), end1) <-
      oneOf
        E.TStart
        [ app start,
          do
            eterm <- term
            end <- getPosition
            commentsAfter <- Space.chomp E.TSpace
            return ((eterm, commentsAfter), end)
        ]
    oneOfWithFallback
      [ do
          Space.checkIndent end1 E.TIndentStart -- should never trigger
          word2 0x2D 0x3E {-->-} E.TStart -- could just be another type instead
          commentsAfterArrow <- Space.chompAndCheckIndent E.TSpace E.TIndentStart
          ((tipe2, commentsAfter), end2) <- expression
          let comments = SC.TLambdaComments commentsBeforeArrow commentsAfterArrow
          let tipe = A.at start end2 (Src.TLambda tipe1 tipe2 comments)
          return ((tipe, commentsAfter), end2)
      ]
      term1

-- TYPE CONSTRUCTORS

app :: A.Position -> Space.Parser E.Type (Src.Type, [Src.Comment])
app start =
  do
    upper <- Var.foreignUpper E.TStart
    upperEnd <- getPosition
    commentsAfterUpper <- Space.chomp E.TSpace
    ((args, commentsAfterArgs), end) <- chompArgs [] commentsAfterUpper upperEnd

    let region = A.Region start upperEnd
    let tipe =
          case upper of
            Var.Unqualified name ->
              Src.TType region name args
            Var.Qualified home name ->
              Src.TTypeQual region home name args

    return ((A.at start end tipe, commentsAfterArgs), end)

chompArgs :: [([Src.Comment], Src.Type)] -> [Src.Comment] -> A.Position -> Space.Parser E.Type ([([Src.Comment], Src.Type)], [Src.Comment])
chompArgs args commentsBetween end =
  oneOfWithFallback
    [ do
        Space.checkIndent end E.TIndentStart
        arg <- term
        newEnd <- getPosition
        commentsAfter <- Space.chomp E.TSpace
        chompArgs ((commentsBetween, arg) : args) commentsAfter newEnd
    ]
    ((reverse args, commentsBetween), end)

-- RECORD

chompRecordEnd :: [Src.TRecordField] -> Parser E.TRecord [Src.TRecordField]
chompRecordEnd fields =
  oneOf
    E.TRecordEnd
    [ do
        word1 0x2C {-,-} E.TRecordEnd
        commentsAfterComma <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField
        field <- chompField commentsAfterComma
        chompRecordEnd (field : fields),
      do
        word1 0x7D {-}-} E.TRecordEnd
        return (reverse fields)
    ]

chompField :: [Src.Comment] -> Parser E.TRecord Src.TRecordField
chompField commentsBefore =
  do
    name <- addLocation (Var.lower E.TRecordField)
    commentsAfterName <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon
    word1 0x3A {-:-} E.TRecordColon
    commentsAfterColon <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType
    ((tipe, commentsAfterTipe), end) <- specialize E.TRecordType expression
    Space.checkIndent end E.TRecordIndentEnd
    let comments = SC.RecordFieldComments commentsBefore commentsAfterName commentsAfterColon commentsAfterTipe
    return (name, tipe, comments)

-- VARIANT

variant :: Space.Parser E.CustomType (A.Located Name.Name, [([Src.Comment], Src.Type)], [Src.Comment])
variant =
  do
    name@(A.At (A.Region _ nameEnd) _) <- addLocation (Var.upper E.CT_Variant)
    commentsBefore <- Space.chomp E.CT_Space
    ((args, commentsAfter), end) <- specialize E.CT_VariantArg (chompArgs [] commentsBefore nameEnd)
    return ((name, args, commentsAfter), end)
