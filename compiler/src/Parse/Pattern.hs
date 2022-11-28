{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}
-- Temporary while implementing gren format
{-# OPTIONS_GHC -Wno-error=unused-do-bind #-}
{-# OPTIONS_GHC -Wno-error=unused-local-binds #-}
{-# OPTIONS_GHC -Wno-error=unused-matches #-}

module Parse.Pattern
  ( term,
    expression,
  )
where

import AST.Source qualified as Src
import AST.SourceComments qualified as SC
import Data.Name qualified as Name
import Data.Utf8 qualified as Utf8
import Foreign.Ptr (plusPtr)
import Parse.Keyword qualified as Keyword
import Parse.Number qualified as Number
import Parse.Primitives (Parser, addEnd, getPosition, inContext, oneOf, oneOfWithFallback, word1)
import Parse.Primitives qualified as P
import Parse.Space qualified as Space
import Parse.String qualified as String
import Parse.Variable qualified as Var
import Reporting.Annotation qualified as A
import Reporting.Error.Syntax qualified as E

-- TERM

term :: Parser E.Pattern Src.Pattern
term =
  do
    start <- getPosition
    oneOf
      E.PStart
      [ record start,
        array start,
        parenthesized,
        termHelp start
      ]

termHelp :: A.Position -> Parser E.Pattern Src.Pattern
termHelp start =
  oneOf
    E.PStart
    [ do
        name <- wildcard
        addEnd start (Src.PAnything name),
      do
        name <- Var.lower E.PStart
        addEnd start (Src.PVar name),
      do
        upper <- Var.foreignUpper E.PStart
        end <- getPosition
        let region = A.Region start end
        return $
          A.at start end $
            case upper of
              Var.Unqualified name ->
                Src.PCtor region name []
              Var.Qualified home name ->
                Src.PCtorQual region home name [],
      do
        number <- Number.number E.PStart E.PNumber
        end <- getPosition
        case number of
          Number.Int int ->
            return (A.at start end (Src.PInt int))
          Number.Float float ->
            P.Parser $ \(P.State _ _ _ _ row col) _ _ cerr _ ->
              let width = fromIntegral (Utf8.size float)
               in cerr row (col - width) (E.PFloat width),
      do
        str <- String.string E.PStart E.PString
        addEnd start (Src.PStr str),
      do
        chr <- String.character E.PStart E.PChar
        addEnd start (Src.PChr chr)
    ]

-- WILDCARD

wildcard :: Parser E.Pattern Name.Name
wildcard =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    if pos == end || P.unsafeIndex pos /= 0x5F {- _ -}
      then eerr row col E.PStart
      else
        let lowerVarPosition = plusPtr pos 1
            (# newPos, newCol #) = Var.chompLower lowerVarPosition end (col + 1)
            -- Note although we are getting the name, to check that it is not a reserved keyword, we are not storing it.
            -- We ultimately wish to throw it away, but in theory we could make the AST of wildcard take the name
            -- as a parameter, and then we could use that, to, for example, check that we are not shadowing/duplicating any
            -- such wildcard names, eg. check against something like:
            -- getZ _x _x z = z
            -- when you probably meant
            -- getZ _x _y z = z
            !name = Name.fromPtr lowerVarPosition newPos
            !newState = P.State src newPos end indent row newCol
         in if Var.isReservedWord name
              then eerr row col E.PStart
              else cok name newState

-- PARENTHESIZED PATTERNS

parenthesized :: Parser E.Pattern Src.Pattern
parenthesized =
  inContext E.PParenthesized (word1 0x28 {-(-} E.PStart) $
    do
      commentsAfterOpenBrace <- Space.chompAndCheckIndent E.PParenthesizedSpace E.PParenthesizedIndentPattern
      ((pattern, commentsAfterPattern), end) <- P.specialize E.PParenthesizedPattern expression
      Space.checkIndent end E.PParenthesizedIndentEnd
      word1 0x29 {-)-} E.PParenthesizedEnd
      return pattern

-- RECORDS

record :: A.Position -> Parser E.Pattern Src.Pattern
record start =
  inContext E.PRecord (word1 0x7B {- { -} E.PStart) $
    do
      Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentOpen
      oneOf
        E.PRecordOpen
        [ do
            word1 0x7D {-}-} E.PRecordEnd
            addEnd start (Src.PRecord []),
          recordPatternHelp start []
        ]

recordPatternHelp :: A.Position -> [Src.RecordFieldPattern] -> Parser E.PRecord Src.Pattern
recordPatternHelp start revPatterns =
  do
    fieldStart <- getPosition
    var <- Var.lower E.PRecordField
    varEnd <- getPosition
    Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentEnd
    oneOf
      E.PRecordEnd
      [ do
          word1 0x3D {-=-} E.PRecordEquals
          commentsAfterEquals <- Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentField
          ((pattern, commentsAfterPattern), fieldEnd) <- P.specialize E.PRecordExpr expression
          Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentEnd
          let namedPattern =
                A.at fieldStart fieldEnd $
                  Src.RFPattern (A.at fieldStart varEnd var) pattern
          recordContinuationHelp start (namedPattern : revPatterns),
        do
          let namedPattern =
                A.at fieldStart varEnd $
                  Src.RFPattern
                    (A.at fieldStart varEnd var)
                    (A.at fieldStart varEnd (Src.PVar var))
          recordContinuationHelp start (namedPattern : revPatterns)
      ]

recordContinuationHelp :: A.Position -> [Src.RecordFieldPattern] -> Parser E.PRecord Src.Pattern
recordContinuationHelp start revPatterns =
  oneOf
    E.PRecordEnd
    [ do
        word1 0x2C {-,-} E.PRecordEnd
        Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentField
        recordPatternHelp start revPatterns,
      do
        word1 0x7D {-}-} E.PRecordEnd
        addEnd start (Src.PRecord (reverse revPatterns))
    ]

-- ARRAY

array :: A.Position -> Parser E.Pattern Src.Pattern
array start =
  inContext E.PArray (word1 0x5B {-[-} E.PStart) $
    do
      commentsAfterOpenBracket <- Space.chompAndCheckIndent E.PArraySpace E.PArrayIndentOpen
      oneOf
        E.PArrayOpen
        [ do
            ((pattern, commentsAfterPattern), end) <- P.specialize E.PArrayExpr expression
            Space.checkIndent end E.PArrayIndentEnd
            let entryComments = SC.PArrayEntryComments commentsAfterOpenBracket commentsAfterPattern
            let entry = (pattern, entryComments)
            arrayHelp start [entry],
          do
            word1 0x5D {-]-} E.PArrayEnd
            addEnd start (Src.PArray [])
        ]

arrayHelp :: A.Position -> [Src.PArrayEntry] -> Parser E.PArray Src.Pattern
arrayHelp start patterns =
  oneOf
    E.PArrayEnd
    [ do
        word1 0x2C {-,-} E.PArrayEnd
        commentsAfterComma <- Space.chompAndCheckIndent E.PArraySpace E.PArrayIndentExpr
        ((pattern, commentsAfterPattern), end) <- P.specialize E.PArrayExpr expression
        Space.checkIndent end E.PArrayIndentEnd
        let entryComments = SC.PArrayEntryComments commentsAfterComma commentsAfterPattern
        let entry = (pattern, entryComments)
        arrayHelp start (entry : patterns),
      do
        word1 0x5D {-]-} E.PArrayEnd
        addEnd start (Src.PArray (reverse patterns))
    ]

-- EXPRESSION

expression :: Space.Parser E.Pattern (Src.Pattern, [Src.Comment])
expression = do
  start <- getPosition
  ePart <- exprPart
  exprHelp start ePart

exprHelp :: A.Position -> ((Src.Pattern, [Src.Comment]), A.Position) -> Space.Parser E.Pattern (Src.Pattern, [Src.Comment])
exprHelp start ((pattern, commentsAfterPattern), end) =
  oneOfWithFallback
    [ do
        Space.checkIndent end E.PIndentStart
        let commentsBeforeAs = commentsAfterPattern
        Keyword.as_ E.PStart
        commentsAfterAs <- Space.chompAndCheckIndent E.PSpace E.PIndentAlias
        nameStart <- getPosition
        name <- Var.lower E.PAlias
        newEnd <- getPosition
        commentsAfterAlias <- Space.chomp E.PSpace
        let alias = A.at nameStart newEnd name
        return
          ( (A.at start newEnd (Src.PAlias pattern alias), commentsAfterAlias),
            newEnd
          )
    ]
    ( (pattern, commentsAfterPattern),
      end
    )

exprPart :: Space.Parser E.Pattern (Src.Pattern, [Src.Comment])
exprPart =
  oneOf
    E.PStart
    [ do
        start <- getPosition
        upper <- Var.foreignUpper E.PStart
        end <- getPosition
        exprTermHelp (A.Region start end) upper start [],
      do
        eterm@(A.At (A.Region _ end) _) <- term
        commentsAfter <- Space.chomp E.PSpace
        return ((eterm, commentsAfter), end)
    ]

exprTermHelp :: A.Region -> Var.Upper -> A.Position -> [([Src.Comment], Src.Pattern)] -> Space.Parser E.Pattern (Src.Pattern, [Src.Comment])
exprTermHelp region upper start revArgs =
  do
    end <- getPosition
    commentsAfter <- Space.chomp E.PSpace
    oneOfWithFallback
      [ do
          Space.checkIndent end E.PIndentStart
          arg <- term
          exprTermHelp region upper start ((commentsAfter, arg) : revArgs)
      ]
      ( ( A.at start end $
            case upper of
              Var.Unqualified name ->
                Src.PCtor region name (reverse revArgs)
              Var.Qualified home name ->
                Src.PCtorQual region home name (reverse revArgs),
          commentsAfter
        ),
        end
      )
