{-# LANGUAGE OverloadedStrings #-}
-- Temporary while implementing gren format
{-# OPTIONS_GHC -Wno-error=unused-do-bind #-}

module Parse.Expression
  ( expression,
  )
where

import AST.Source qualified as Src
import Data.Name qualified as Name
import Parse.Keyword qualified as Keyword
import Parse.Number qualified as Number
import Parse.Pattern qualified as Pattern
import Parse.Primitives hiding (State)
import Parse.Primitives qualified as P
import Parse.Space qualified as Space
import Parse.String qualified as String
import Parse.Symbol qualified as Symbol
import Parse.Type qualified as Type
import Parse.Variable qualified as Var
import Reporting.Annotation qualified as A
import Reporting.Error.Syntax qualified as E

-- TERMS

term :: Parser E.Expr Src.Expr
term =
  do
    start <- getPosition
    oneOf
      E.Start
      [ variable start >>= accessible start,
        string start,
        number start,
        parenthesizedExpr start >>= accessible start,
        array start,
        record start >>= accessible start,
        accessor start,
        character start,
        wildcard
      ]

string :: A.Position -> Parser E.Expr Src.Expr
string start =
  do
    str <- String.string E.Start E.String
    addEnd start (Src.Str str)

character :: A.Position -> Parser E.Expr Src.Expr
character start =
  do
    chr <- String.character E.Start E.Char
    addEnd start (Src.Chr chr)

number :: A.Position -> Parser E.Expr Src.Expr
number start =
  do
    nmbr <- Number.number E.Start E.Number
    addEnd start $
      case nmbr of
        Number.Int int -> Src.Int int
        Number.Float float -> Src.Float float

parenthesizedExpr :: A.Position -> Parser E.Expr Src.Expr
parenthesizedExpr start@(A.Position row col) =
  inContext E.Parenthesized (word1 0x28 {-(-} E.Start) $
    do
      comments1 <- Space.chompAndCheckIndent E.ParenthesizedSpace E.ParenthesizedIndentOpen
      oneOf
        E.ParenthesizedOpen
        [ do
            op <- Symbol.operator E.ParenthesizedOpen E.ParenthesizedOperatorReserved
            if op == "-"
              then
                oneOf
                  E.ParenthesizedOperatorClose
                  [ do
                      word1 0x29 {-)-} E.ParenthesizedOperatorClose
                      addEnd start (Src.Op op),
                    do
                      (comments2, (expr, end)) <-
                        specialize E.ParenthesizedExpr $
                          do
                            negatedExpr@(A.At (A.Region _ end) _) <- term
                            comments2_ <- Space.chomp E.Space
                            let exprStart = A.Position row (col + 2)
                            let expr = A.at exprStart end (Src.Negate negatedExpr)
                            (,) comments2_ <$> chompExprEnd exprStart (State [] expr [] end)
                      Space.checkIndent end E.ParenthesizedIndentEnd
                      word1 0x29 {-)-} E.ParenthesizedOperatorClose
                      addEnd start (Src.Parens comments1 expr comments2)
                  ]
              else do
                word1 0x29 {-)-} E.ParenthesizedOperatorClose
                addEnd start (Src.Op op),
          do
            (expr, _) <- specialize E.ParenthesizedExpr expression
            word1 0x29 {-)-} E.ParenthesizedEnd
            addEnd start (Src.Parens comments1 expr [])
        ]

accessor :: A.Position -> Parser E.Expr Src.Expr
accessor start =
  do
    word1 0x2E {-.-} E.Dot
    field <- Var.lower E.Access
    addEnd start (Src.Accessor field)

variable :: A.Position -> Parser E.Expr Src.Expr
variable start =
  do
    var <- Var.foreignAlpha E.Start
    addEnd start var

wildcard :: Parser E.Expr a
wildcard =
  do
    word1 0x5F {- _ -} E.Start
    -- Note, because this is not optional, this will not match '_' on its own.
    name <- Var.lower E.Start
    P.Parser $ \(P.State _ _ _ _ row col) _ _ cerr _ ->
      cerr row col (E.WildCard $ E.WildCardAttempt name)

accessible :: A.Position -> Src.Expr -> Parser E.Expr Src.Expr
accessible start expr =
  oneOfWithFallback
    [ do
        word1 0x2E {-.-} E.Dot
        pos <- getPosition
        field <- Var.lower E.Access
        end <- getPosition
        accessible start $
          A.at start end (Src.Access expr (A.at pos end field))
    ]
    expr

-- ARRAYS

array :: A.Position -> Parser E.Expr Src.Expr
array start =
  inContext E.Array (word1 0x5B {-[-} E.Start) $
    do
      _ <- Space.chompAndCheckIndent E.ArraySpace E.ArrayIndentOpen
      oneOf
        E.ArrayOpen
        [ do
            (entry, end) <- specialize E.ArrayExpr expression
            Space.checkIndent end E.ArrayIndentEnd
            chompArrayEnd start [entry],
          do
            word1 0x5D {-]-} E.ArrayOpen
            addEnd start (Src.Array [])
        ]

chompArrayEnd :: A.Position -> [Src.Expr] -> Parser E.Array Src.Expr
chompArrayEnd start entries =
  oneOf
    E.ArrayEnd
    [ do
        word1 0x2C {-,-} E.ArrayEnd
        _ <- Space.chompAndCheckIndent E.ArraySpace E.ArrayIndentExpr
        (entry, end) <- specialize E.ArrayExpr expression
        Space.checkIndent end E.ArrayIndentEnd
        chompArrayEnd start (entry : entries),
      do
        word1 0x5D {-]-} E.ArrayEnd
        addEnd start (Src.Array (reverse entries))
    ]

-- RECORDS

record :: A.Position -> Parser E.Expr Src.Expr
record start =
  inContext E.Record (word1 0x7B {- { -} E.Start) $
    do
      _ <- Space.chompAndCheckIndent E.RecordSpace E.RecordIndentOpen
      oneOf
        E.RecordOpen
        [ do
            word1 0x7D {-}-} E.RecordEnd
            addEnd start (Src.Record []),
          do
            expr <- specialize E.RecordUpdateExpr term
            _ <- Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
            oneOf
              E.RecordEquals
              [ do
                  word1 0x7C {- vertical bar -} E.RecordPipe
                  _ <- Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField
                  firstField <- chompField
                  fields <- chompFields [firstField]
                  addEnd start (Src.Update expr fields),
                do
                  word1 0x3D {-=-} E.RecordEquals
                  _ <- Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr
                  (value, end) <- specialize E.RecordExpr expression
                  Space.checkIndent end E.RecordIndentEnd
                  case expr of
                    A.At exprRegion (Src.Var Src.LowVar name) -> do
                      fields <- chompFields [(A.At exprRegion name, value)]
                      addEnd start (Src.Record fields)
                    A.At (A.Region (A.Position row col) _) _ ->
                      P.Parser $ \_ _ _ _ eerr ->
                        eerr row col E.RecordField
              ]
        ]

type Field = (A.Located Name.Name, Src.Expr)

chompFields :: [Field] -> Parser E.Record [Field]
chompFields fields =
  oneOf
    E.RecordEnd
    [ do
        word1 0x2C {-,-} E.RecordEnd
        _ <- Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField
        f <- chompField
        chompFields (f : fields),
      do
        word1 0x7D {-}-} E.RecordEnd
        return (reverse fields)
    ]

chompField :: Parser E.Record Field
chompField =
  do
    key <- addLocation (Var.lower E.RecordField)
    _ <- Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
    word1 0x3D {-=-} E.RecordEquals
    _ <- Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr
    (value, end) <- specialize E.RecordExpr expression
    Space.checkIndent end E.RecordIndentEnd
    return (key, value)

-- EXPRESSIONS

expression :: Space.Parser E.Expr Src.Expr
expression =
  do
    start <- getPosition
    oneOf
      E.Start
      [ let_ start,
        if_ start,
        case_ start,
        function start,
        do
          expr <- possiblyNegativeTerm start
          end <- getPosition
          _ <- Space.chomp E.Space
          chompExprEnd start (State [] expr [] end)
      ]

data State = State
  { _ops :: ![(Src.Expr, A.Located Name.Name)],
    _expr :: !Src.Expr,
    _args :: ![Src.Expr],
    _end :: !A.Position
  }

chompExprEnd :: A.Position -> State -> Space.Parser E.Expr Src.Expr
chompExprEnd start (State ops expr args end) =
  oneOfWithFallback
    [ -- argument
      do
        Space.checkIndent end E.Start
        arg <- term
        newEnd <- getPosition
        _ <- Space.chomp E.Space
        chompExprEnd start (State ops expr (arg : args) newEnd),
      -- operator
      do
        Space.checkIndent end E.Start
        op@(A.At (A.Region opStart opEnd) opName) <- addLocation (Symbol.operator E.Start E.OperatorReserved)
        _ <- Space.chompAndCheckIndent E.Space (E.IndentOperatorRight opName)
        newStart <- getPosition
        if "-" == opName && end /= opStart && opEnd == newStart
          then -- negative terms
          do
            negatedExpr <- term
            newEnd <- getPosition
            _ <- Space.chomp E.Space
            let arg = A.at opStart newEnd (Src.Negate negatedExpr)
            chompExprEnd start (State ops expr (arg : args) newEnd)
          else
            let err = E.OperatorRight opName
             in oneOf
                  err
                  [ -- term
                    do
                      newExpr <- possiblyNegativeTerm newStart
                      newEnd <- getPosition
                      _ <- Space.chomp E.Space
                      let newOps = (toCall expr args, op) : ops
                      chompExprEnd start (State newOps newExpr [] newEnd),
                    -- final term
                    do
                      (newLast, newEnd) <-
                        oneOf
                          err
                          [ let_ newStart,
                            case_ newStart,
                            if_ newStart,
                            function newStart
                          ]
                      let newOps = (toCall expr args, op) : ops
                      let finalExpr = Src.Binops (reverse newOps) newLast
                      return (A.at start newEnd finalExpr, newEnd)
                  ]
    ]
    -- done
    ( case ops of
        [] ->
          ( toCall expr args,
            end
          )
        _ ->
          ( A.at start end (Src.Binops (reverse ops) (toCall expr args)),
            end
          )
    )

possiblyNegativeTerm :: A.Position -> Parser E.Expr Src.Expr
possiblyNegativeTerm start =
  oneOf
    E.Start
    [ do
        word1 0x2D {---} E.Start
        expr <- term
        addEnd start (Src.Negate expr),
      term
    ]

toCall :: Src.Expr -> [Src.Expr] -> Src.Expr
toCall func revArgs =
  case revArgs of
    [] ->
      func
    lastArg : _ ->
      A.merge func lastArg (Src.Call func (reverse revArgs))

-- IF EXPRESSION

if_ :: A.Position -> Space.Parser E.Expr Src.Expr
if_ start =
  inContext E.If (Keyword.if_ E.Start) $
    chompIfEnd start []

chompIfEnd :: A.Position -> [(Src.Expr, Src.Expr)] -> Space.Parser E.If Src.Expr
chompIfEnd start branches =
  do
    _ <- Space.chompAndCheckIndent E.IfSpace E.IfIndentCondition
    (condition, condEnd) <- specialize E.IfCondition expression
    Space.checkIndent condEnd E.IfIndentThen
    Keyword.then_ E.IfThen
    _ <- Space.chompAndCheckIndent E.IfSpace E.IfIndentThenBranch
    (thenBranch, thenEnd) <- specialize E.IfThenBranch expression
    Space.checkIndent thenEnd E.IfIndentElse
    Keyword.else_ E.IfElse
    _ <- Space.chompAndCheckIndent E.IfSpace E.IfIndentElseBranch
    let newBranches = (condition, thenBranch) : branches
    oneOf
      E.IfElseBranchStart
      [ do
          Keyword.if_ E.IfElseBranchStart
          chompIfEnd start newBranches,
        do
          (elseBranch, elseEnd) <- specialize E.IfElseBranch expression
          let ifExpr = Src.If (reverse newBranches) elseBranch
          return (A.at start elseEnd ifExpr, elseEnd)
      ]

-- LAMBDA EXPRESSION

function :: A.Position -> Space.Parser E.Expr Src.Expr
function start =
  inContext E.Func (word1 0x5C {-\-} E.Start) $
    do
      _ <- Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArg
      arg <- specialize E.FuncArg Pattern.term
      _ <- Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
      revArgs <- chompArgs [arg]
      _ <- Space.chompAndCheckIndent E.FuncSpace E.FuncIndentBody
      (body, end) <- specialize E.FuncBody expression
      let funcExpr = Src.Lambda (reverse revArgs) body
      return (A.at start end funcExpr, end)

chompArgs :: [Src.Pattern] -> Parser E.Func [Src.Pattern]
chompArgs revArgs =
  oneOf
    E.FuncArrow
    [ do
        arg <- specialize E.FuncArg Pattern.term
        _ <- Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
        chompArgs (arg : revArgs),
      do
        word2 0x2D 0x3E {-->-} E.FuncArrow
        return revArgs
    ]

-- CASE EXPRESSIONS

case_ :: A.Position -> Space.Parser E.Expr Src.Expr
case_ start =
  inContext E.Case (Keyword.case_ E.Start) $
    do
      _ <- Space.chompAndCheckIndent E.CaseSpace E.CaseIndentExpr
      (expr, exprEnd) <- specialize E.CaseExpr expression
      Space.checkIndent exprEnd E.CaseIndentOf
      Keyword.of_ E.CaseOf
      _ <- Space.chompAndCheckIndent E.CaseSpace E.CaseIndentPattern
      withIndent $
        do
          (firstBranch, firstEnd) <- chompBranch
          (branches, end) <- chompCaseEnd [firstBranch] firstEnd
          return
            ( A.at start end (Src.Case expr branches),
              end
            )

chompBranch :: Space.Parser E.Case (Src.Pattern, Src.Expr)
chompBranch =
  do
    (pattern, patternEnd) <- specialize E.CasePattern Pattern.expression
    Space.checkIndent patternEnd E.CaseIndentArrow
    word2 0x2D 0x3E {-->-} E.CaseArrow
    _ <- Space.chompAndCheckIndent E.CaseSpace E.CaseIndentBranch
    (branchExpr, end) <- specialize E.CaseBranch expression
    return ((pattern, branchExpr), end)

chompCaseEnd :: [(Src.Pattern, Src.Expr)] -> A.Position -> Space.Parser E.Case [(Src.Pattern, Src.Expr)]
chompCaseEnd branches end =
  oneOfWithFallback
    [ do
        Space.checkAligned E.CasePatternAlignment
        (branch, newEnd) <- chompBranch
        chompCaseEnd (branch : branches) newEnd
    ]
    (reverse branches, end)

-- LET EXPRESSION

let_ :: A.Position -> Space.Parser E.Expr Src.Expr
let_ start =
  inContext E.Let (Keyword.let_ E.Start) $
    do
      (defs, defsEnd) <-
        withBacksetIndent 3 $
          do
            _ <- Space.chompAndCheckIndent E.LetSpace E.LetIndentDef
            withIndent $
              do
                (def, end) <- chompLetDef
                chompLetDefs [def] end

      Space.checkIndent defsEnd E.LetIndentIn
      Keyword.in_ E.LetIn
      _ <- Space.chompAndCheckIndent E.LetSpace E.LetIndentBody
      (body, end) <- specialize E.LetBody expression
      return
        ( A.at start end (Src.Let defs body),
          end
        )

chompLetDefs :: [A.Located Src.Def] -> A.Position -> Space.Parser E.Let [A.Located Src.Def]
chompLetDefs revDefs end =
  oneOfWithFallback
    [ do
        Space.checkAligned E.LetDefAlignment
        (def, newEnd) <- chompLetDef
        chompLetDefs (def : revDefs) newEnd
    ]
    (reverse revDefs, end)

-- LET DEFINITIONS

chompLetDef :: Space.Parser E.Let (A.Located Src.Def)
chompLetDef =
  oneOf
    E.LetDefName
    [ definition,
      destructure
    ]

-- DEFINITION

definition :: Space.Parser E.Let (A.Located Src.Def)
definition =
  do
    aname@(A.At (A.Region start _) name) <- addLocation (Var.lower E.LetDefName)
    specialize (E.LetDef name) $
      do
        _ <- Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
        oneOf
          E.DefEquals
          [ do
              word1 0x3A {-:-} E.DefEquals
              _ <- Space.chompAndCheckIndent E.DefSpace E.DefIndentType
              (tipe, _) <- specialize E.DefType Type.expression
              Space.checkAligned E.DefAlignment
              defName <- chompMatchingName name
              _ <- Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
              chompDefArgsAndBody start defName (Just tipe) [],
            chompDefArgsAndBody start aname Nothing []
          ]

chompDefArgsAndBody :: A.Position -> A.Located Name.Name -> Maybe Src.Type -> [Src.Pattern] -> Space.Parser E.Def (A.Located Src.Def)
chompDefArgsAndBody start name tipe revArgs =
  oneOf
    E.DefEquals
    [ do
        arg <- specialize E.DefArg Pattern.term
        _ <- Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
        chompDefArgsAndBody start name tipe (arg : revArgs),
      do
        word1 0x3D {-=-} E.DefEquals
        _ <- Space.chompAndCheckIndent E.DefSpace E.DefIndentBody
        (body, end) <- specialize E.DefBody expression
        return
          ( A.at start end (Src.Define name (reverse revArgs) body tipe),
            end
          )
    ]

chompMatchingName :: Name.Name -> Parser E.Def (A.Located Name.Name)
chompMatchingName expectedName =
  let (P.Parser parserL) = Var.lower E.DefNameRepeat
   in P.Parser $ \state@(P.State _ _ _ _ sr sc) cok eok cerr eerr ->
        let cokL name newState@(P.State _ _ _ _ er ec) =
              if expectedName == name
                then cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState
                else cerr sr sc (E.DefNameMatch name)

            eokL name newState@(P.State _ _ _ _ er ec) =
              if expectedName == name
                then eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState
                else eerr sr sc (E.DefNameMatch name)
         in parserL state cokL eokL cerr eerr

-- DESTRUCTURE

destructure :: Space.Parser E.Let (A.Located Src.Def)
destructure =
  specialize E.LetDestruct $
    do
      start <- getPosition
      pattern <- specialize E.DestructPattern Pattern.term
      _ <- Space.chompAndCheckIndent E.DestructSpace E.DestructIndentEquals
      word1 0x3D {-=-} E.DestructEquals
      _ <- Space.chompAndCheckIndent E.DestructSpace E.DestructIndentBody
      (expr, end) <- specialize E.DestructBody expression
      return (A.at start end (Src.Destruct pattern expr), end)
