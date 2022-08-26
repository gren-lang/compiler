{-# LANGUAGE OverloadedStrings #-}
-- Temporary while implementing gren format
{-# OPTIONS_GHC -Wno-error=unused-do-bind #-}

module Parse.Type
  ( expression,
    variant,
  )
where

import AST.Source qualified as Src
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
            _ <- Space.chompAndCheckIndent E.TParenthesisSpace E.TParenthesisIndentOpen
            (tipe, end) <- specialize E.TParenthesisType expression
            Space.checkIndent end E.TParenthesisIndentEnd
            word1 0x29 {-)-} E.TParenthesisEnd
            return tipe,
        -- records
        inContext E.TRecord (word1 0x7B {- { -} E.TStart) $
          do
            _ <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentOpen
            oneOf
              E.TRecordOpen
              [ do
                  word1 0x7D {-}-} E.TRecordEnd
                  addEnd start (Src.TRecord [] Nothing),
                do
                  name <- addLocation (Var.lower E.TRecordField)
                  _ <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon
                  oneOf
                    E.TRecordColon
                    [ do
                        word1 0x7C E.TRecordColon
                        _ <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField
                        field <- chompField
                        fields <- chompRecordEnd [field]
                        addEnd start (Src.TRecord fields (Just name)),
                      do
                        word1 0x3A {-:-} E.TRecordColon
                        _ <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType
                        (tipe, end) <- specialize E.TRecordType expression
                        Space.checkIndent end E.TRecordIndentEnd
                        fields <- chompRecordEnd [(name, tipe)]
                        addEnd start (Src.TRecord fields Nothing)
                    ]
              ]
      ]

-- TYPE EXPRESSIONS

expression :: Space.Parser E.Type Src.Type
expression =
  do
    start <- getPosition
    term1@(tipe1, end1) <-
      oneOf
        E.TStart
        [ app start,
          do
            eterm <- term
            end <- getPosition
            _ <- Space.chomp E.TSpace
            return (eterm, end)
        ]
    oneOfWithFallback
      [ do
          Space.checkIndent end1 E.TIndentStart -- should never trigger
          word2 0x2D 0x3E {-->-} E.TStart -- could just be another type instead
          _ <- Space.chompAndCheckIndent E.TSpace E.TIndentStart
          (tipe2, end2) <- expression
          let tipe = A.at start end2 (Src.TLambda tipe1 tipe2)
          return (tipe, end2)
      ]
      term1

-- TYPE CONSTRUCTORS

app :: A.Position -> Space.Parser E.Type Src.Type
app start =
  do
    upper <- Var.foreignUpper E.TStart
    upperEnd <- getPosition
    _ <- Space.chomp E.TSpace
    (args, end) <- chompArgs [] upperEnd

    let region = A.Region start upperEnd
    let tipe =
          case upper of
            Var.Unqualified name ->
              Src.TType region name args
            Var.Qualified home name ->
              Src.TTypeQual region home name args

    return (A.at start end tipe, end)

chompArgs :: [Src.Type] -> A.Position -> Space.Parser E.Type [Src.Type]
chompArgs args end =
  oneOfWithFallback
    [ do
        Space.checkIndent end E.TIndentStart
        arg <- term
        newEnd <- getPosition
        _ <- Space.chomp E.TSpace
        chompArgs (arg : args) newEnd
    ]
    (reverse args, end)

-- RECORD

type Field = (A.Located Name.Name, Src.Type)

chompRecordEnd :: [Field] -> Parser E.TRecord [Field]
chompRecordEnd fields =
  oneOf
    E.TRecordEnd
    [ do
        word1 0x2C {-,-} E.TRecordEnd
        _ <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField
        field <- chompField
        chompRecordEnd (field : fields),
      do
        word1 0x7D {-}-} E.TRecordEnd
        return (reverse fields)
    ]

chompField :: Parser E.TRecord Field
chompField =
  do
    name <- addLocation (Var.lower E.TRecordField)
    _ <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon
    word1 0x3A {-:-} E.TRecordColon
    _ <- Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType
    (tipe, end) <- specialize E.TRecordType expression
    Space.checkIndent end E.TRecordIndentEnd
    return (name, tipe)

-- VARIANT

variant :: Space.Parser E.CustomType (A.Located Name.Name, [Src.Type])
variant =
  do
    name@(A.At (A.Region _ nameEnd) _) <- addLocation (Var.upper E.CT_Variant)
    _ <- Space.chomp E.CT_Space
    (args, end) <- specialize E.CT_VariantArg (chompArgs [] nameEnd)
    return ((name, args), end)
