{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-error=unused-matches #-}

module Gren.Format (toByteStringBuilder) where

import AST.Source qualified as Src
import AST.SourceComments qualified as SC
import AST.Utils.Binop qualified as Binop
import Control.Monad (join)
import Data.Bifunctor (second)
import Data.ByteString.Builder qualified as B
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Name (Name)
import Data.Semigroup (sconcat)
import Data.Utf8 qualified as Utf8
import Parse.Primitives qualified as P
import Reporting.Annotation qualified as A
import Text.PrettyPrint.Avh4.Block (Block)
import Text.PrettyPrint.Avh4.Block qualified as Block

toByteStringBuilder :: Src.Module -> B.Builder
toByteStringBuilder module_ =
  Block.render (formatModule module_)

--
-- Data structure extras
--

repair3 :: [(a, b, c)] -> a -> (a, [(b, c, a)])
repair3 [] single = (single, [])
repair3 ((first, b, c) : rest) final =
  (first, repair3Help b c rest final)

repair3Help :: b -> c -> [(a, b, c)] -> a -> [(b, c, a)]
repair3Help b c [] a = [(b, c, a)]
repair3Help b1 c1 ((a1, b2, c2) : rest) a2 =
  (b1, c1, a1) : repair3Help b2 c2 rest a2

--
-- Helper functions
--

utf8 :: Utf8.Utf8 any -> Block.Line
utf8 = Block.lineFromBuilder . Utf8.toBuilder

addBlankLines :: Int -> Block -> Block
addBlankLines n block =
  Block.stack $
    NonEmpty.prependList
      (replicate n Block.blankLine)
      (NonEmpty.singleton block)

spaceOrStack :: NonEmpty Block -> Block
spaceOrStack = Block.rowOrStack (Just Block.space)

spaceOrIndent :: NonEmpty Block -> Block
spaceOrIndent = Block.rowOrIndent (Just Block.space)

spaceOrIndentForce :: Bool -> NonEmpty Block -> Block
spaceOrIndentForce forceMultiline = Block.rowOrIndentForce forceMultiline (Just Block.space)

group :: Char -> Char -> Char -> Bool -> [Block] -> Block
group open _ close _ [] = Block.line $ Block.char7 open <> Block.char7 close
group open sep close forceMultiline (first : rest) =
  Block.rowOrStackForce
    forceMultiline
    (Just Block.space)
    [ Block.rowOrStackForce forceMultiline Nothing $
        Block.prefix 2 (Block.char7 open <> Block.space) first
          :| fmap (Block.prefix 2 (Block.char7 sep <> Block.space)) (rest),
      Block.line (Block.char7 close)
    ]

surround :: Char -> Char -> Block -> Block
surround open close block =
  Block.rowOrStack
    Nothing
    [ Block.prefix 1 (Block.char7 open) block,
      Block.line $ Block.char7 close
    ]

parens :: Block -> Block
parens = surround '(' ')'

extendedGroup :: Char -> Char -> Char -> Char -> Char -> Block -> NonEmpty (Block.Line, Block) -> Block
extendedGroup open baseSep sep fieldSep close base fields =
  case fields of
    (single :| []) ->
      spaceOrStack
        [ spaceOrIndent
            [ spaceOrIndent
                [ Block.line $ Block.char7 open,
                  base
                ],
              formatField baseSep single
            ],
          Block.line $ Block.char7 close
        ]
    (first :| rest) ->
      Block.stack
        [ spaceOrIndent
            [ Block.line $ Block.char7 open,
              base
            ],
          Block.indent $
            Block.stack $
              formatField baseSep first
                :| fmap (formatField sep) rest,
          Block.line $ Block.char7 close
        ]
  where
    formatField punc (key, value) =
      spaceOrIndent
        [ Block.line $
            Block.char7 punc
              <> Block.space
              <> key
              <> Block.space
              <> Block.char7 fieldSep,
          value
        ]

withCommentsBefore :: [Src.Comment] -> Block -> Block
withCommentsBefore before = withCommentsAround before []

withCommentsAround :: [Src.Comment] -> [Src.Comment] -> Block -> Block
withCommentsAround [] [] block = block
withCommentsAround before after block =
  case (formatCommentBlock before, formatCommentBlock after) of
    (Nothing, Nothing) -> block
    (Just beforeBlock, Nothing) ->
      spaceOrStack [beforeBlock, block]
    (Nothing, Just afterBlock) ->
      spaceOrIndent [block, afterBlock]
    (Just beforeBlock, Just afterBlock) ->
      spaceOrStack [beforeBlock, spaceOrIndent [block, afterBlock]]

withCommentsStackBefore :: [Src.Comment] -> Block -> Block
withCommentsStackBefore before = withCommentsStackAround before []

withCommentsStackAround :: [Src.Comment] -> [Src.Comment] -> Block -> Block
withCommentsStackAround [] [] block = block
withCommentsStackAround before after block =
  case (formatCommentBlock before, formatCommentBlock after) of
    (Nothing, Nothing) -> block
    (Just beforeBlock, Nothing) -> Block.stack [beforeBlock, block]
    (Nothing, Just afterBlock) -> Block.stack [block, afterBlock]
    (Just beforeBlock, Just afterBlock) -> Block.stack [beforeBlock, block, afterBlock]

--
-- AST -> Block
--

formatComment :: Src.Comment -> Block
formatComment = \case
  A.At _ (Src.BlockComment text) ->
    let open = if Utf8.startsWithChar (== ' ') text then "{-" else "{- "
        close = if Utf8.endsWithWord8 0x20 {- space -} text then "-}" else " -}"
     in Block.line $ Block.string7 open <> utf8 text <> Block.string7 close
  A.At _ (Src.LineComment text) ->
    let open = if Utf8.startsWithChar (== ' ') text then "--" else "-- "
     in Block.mustBreak $ Block.string7 open <> utf8 text

formatCommentBlock :: [Src.Comment] -> Maybe Block
formatCommentBlock =
  fmap formatCommentBlockNonEmpty . nonEmpty

formatCommentBlockNonEmpty :: NonEmpty Src.Comment -> Block
formatCommentBlockNonEmpty =
  spaceOrStack . fmap formatComment

formatModule :: Src.Module -> Block
formatModule (Src.Module moduleName exports docs imports values unions aliases binops topLevelComments comments effects) =
  Block.stack $
    NonEmpty.fromList $
      catMaybes
        [ formatCommentBlock commentsBeforeLine,
          Just $
            spaceOrIndent $
              NonEmpty.fromList $
                catMaybes
                  [ Just $ Block.line $ Block.string7 moduleKeyword,
                    formatCommentBlock commentsAfterKeyword,
                    Just $ Block.line $ maybe (Block.string7 "Main") (utf8 . A.toValue) moduleName,
                    formatCommentBlock commentsAfterName,
                    formatEffectsModuleWhereClause effects,
                    formatExposing commentsAfterExposingKeyword [] (A.toValue exports)
                  ],
          case docs of
            Src.NoDocs _ -> Nothing
            Src.YesDocs moduleDocs _ ->
              Just $
                Block.stack
                  [ Block.blankLine,
                    formatDocComment moduleDocs
                  ],
          formatCommentBlock (commentsAfterLine <> commentsAfterDocComment),
          Just $ Block.stack $ Block.blankLine :| fmap formatImport imports,
          infixDefs,
          let defs =
                fmap snd $
                  List.sortOn fst $
                    concat @[]
                      [ fmap (formatWithDocComment valueName formatValue . A.toValue) <$> values,
                        fmap (formatWithDocComment unionName formatUnion . A.toValue) <$> unions,
                        fmap (formatWithDocComment aliasName formatAlias . A.toValue) <$> aliases,
                        fmap formatTopLevelCommentBlock <$> topLevelComments,
                        case effects of
                          Src.NoEffects -> []
                          Src.Ports ports _ -> fmap (formatWithDocComment portName formatPort) <$> ports
                          Src.Manager _ _ _ -> []
                      ]
           in fmap Block.stack $ nonEmpty $ fmap (addBlankLines 2) defs
        ]
  where
    (SC.HeaderComments commentsBeforeLine commentsAfterKeyword commentsAfterName commentsAfterExposingKeyword commentsAfterLine commentsAfterDocComment) = comments

    moduleKeyword =
      case effects of
        Src.NoEffects -> "module"
        Src.Ports _ (SC.PortsComments afterPortKeyword) -> "port module"
        Src.Manager _ _ (SC.ManagerComments afterEffectKeyword _ _) -> "effect module"

    defDocs :: Map Name Src.DocComment
    defDocs =
      case docs of
        Src.NoDocs _ -> Map.empty
        Src.YesDocs _ defs -> Map.fromList defs

    valueName (Src.Value name _ _ _ _) = A.toValue name
    unionName (Src.Union name _ _) = A.toValue name
    aliasName (Src.Alias name _ _) = A.toValue name
    portName (Src.Port name _) = A.toValue name

    formatWithDocComment :: (a -> Name) -> (a -> Block) -> a -> Block
    formatWithDocComment getName render a =
      case Map.lookup (getName a) defDocs of
        Nothing -> render a
        Just defDoc ->
          Block.stack
            [ formatDocComment defDoc,
              render a
            ]

    infixDefs =
      case NonEmpty.nonEmpty binops of
        Nothing -> Nothing
        Just some ->
          Just $
            Block.stack
              [ Block.blankLine,
                Block.stack $ fmap (formatInfix . A.toValue) some
              ]

formatTopLevelCommentBlock :: NonEmpty Src.Comment -> Block
formatTopLevelCommentBlock comments =
  Block.stack
    [ Block.blankLine,
      formatCommentBlockNonEmpty comments
    ]

formatEffectsModuleWhereClause :: Src.Effects -> Maybe Block
formatEffectsModuleWhereClause = \case
  Src.NoEffects -> Nothing
  Src.Ports _ _ -> Nothing
  Src.Manager _ manager (SC.ManagerComments _ afterWhereKeyword afterManager) ->
    Just $ formatManager manager

formatManager :: Src.Manager -> Block
formatManager manager =
  spaceOrIndent
    [ Block.line $ Block.string7 "where",
      group '{' ',' '}' False $
        fmap (formatPair . second A.toValue) $
          case manager of
            Src.Cmd cmd (SC.CmdComments comments1 comments2) ->
              [(comments1 ++ comments2, "command", cmd)]
            Src.Sub sub (SC.SubComments comments1 comments2) ->
              [(comments1 ++ comments2, "subscription", sub)]
            Src.Fx cmd sub (SC.FxComments (SC.CmdComments commentsCmd1 commentsCmd2) (SC.SubComments commentsSub1 commentsSub2)) ->
              [ (commentsCmd1 ++ commentsCmd2, "command", cmd),
                (commentsSub1 ++ commentsSub2, "subscription", sub)
              ]
    ]
  where
    formatPair (comments, key, name) =
      spaceOrStack $
        NonEmpty.prependList
          (fmap formatComment comments)
          ( NonEmpty.singleton $
              Block.line $
                sconcat
                  [ Block.string7 key,
                    Block.string7 " = ",
                    utf8 name
                  ]
          )

formatExposing :: [Src.Comment] -> [Src.Comment] -> Src.Exposing -> Maybe Block
formatExposing commentsAfterKeyword commentsAfterListing = \case
  Src.Open ->
    Just $
      spaceOrIndent
        [ Block.line $ Block.string7 "exposing",
          withCommentsAround commentsAfterKeyword commentsAfterListing $
            Block.line $
              Block.string7 "(..)"
        ]
  Src.Explicit [] ->
    formatCommentBlock (commentsAfterKeyword <> commentsAfterListing)
  Src.Explicit exposed ->
    Just $
      spaceOrIndent
        [ Block.line $ Block.string7 "exposing",
          withCommentsAround commentsAfterKeyword commentsAfterListing $
            group '(' ',' ')' False $
              fmap formatExposed exposed
        ]

formatExposed :: Src.Exposed -> Block
formatExposed = \case
  Src.Lower name -> Block.line $ utf8 $ A.toValue name
  Src.Upper name privacy -> Block.line $ utf8 $ A.toValue name
  Src.Operator _ name -> Block.line $ Block.char7 '(' <> utf8 name <> Block.char7 ')'

formatImport :: ([Src.Comment], Src.Import) -> Block
formatImport (commentsBefore, Src.Import name alias exposing exposingComments comments) =
  let (SC.ImportComments commentsAfterKeyword commentsAfterName) = comments
   in spaceOrIndent $
        NonEmpty.fromList $
          catMaybes
            [ Just $ Block.line $ Block.string7 "import",
              Just $ withCommentsBefore commentsAfterKeyword $ Block.line $ utf8 $ A.toValue name,
              (spaceOrStack . fmap formatComment) <$> NonEmpty.nonEmpty commentsAfterName,
              fmap formatImportAlias alias,
              formatExposing
                (maybe [] SC._afterExposing exposingComments)
                (maybe [] SC._afterExposingListing exposingComments)
                exposing
            ]

formatImportAlias :: (Name, SC.ImportAliasComments) -> Block
formatImportAlias (name, SC.ImportAliasComments afterAs afterAliasName) =
  spaceOrIndent
    [ Block.line $ Block.string7 "as",
      withCommentsAround afterAs afterAliasName (Block.line $ utf8 name)
    ]

formatDocComment :: Src.DocComment -> Block
formatDocComment (Src.DocComment doc) =
  Block.line $
    Block.string7 "{-|"
      <> Block.lineFromBuilder (P.snippetToBuilder doc)
      <> Block.string7 "-}"

formatInfix :: Src.Infix -> Block
formatInfix (Src.Infix name assoc (Binop.Precedence prec) fn) =
  Block.line $
    Block.string7 "infix "
      <> formatAssociativity assoc
      <> Block.space
      <> Block.string7 (show prec)
      <> Block.string7 " ("
      <> utf8 name
      <> Block.string7 ") = "
      <> utf8 fn

formatAssociativity :: Binop.Associativity -> Block.Line
formatAssociativity = \case
  Binop.Left -> Block.string7 "left "
  Binop.Non -> Block.string7 "non  "
  Binop.Right -> Block.string7 "right"

formatValue :: Src.Value -> Block
formatValue (Src.Value name args body type_ comments) =
  formatBasicDef (A.toValue name) args (A.toValue body) (fmap A.toValue type_) comments

formatBasicDef :: Name -> [([Src.Comment], Src.Pattern)] -> Src.Expr_ -> Maybe Src.Type_ -> SC.ValueComments -> Block
formatBasicDef name args body type_ (SC.ValueComments commentsBeforeEquals commentsBeforeBody commentsAfterBody) =
  Block.stack $
    NonEmpty.fromList $
      catMaybes
        [ fmap (formatTypeAnnotation Nothing name) type_,
          Just $
            spaceOrIndent $
              Block.line (utf8 name)
                :| fmap formatPat args
                ++ [ withCommentsBefore commentsBeforeEquals $
                       Block.line (Block.char7 '=')
                   ],
          Just $
            Block.indent $
              withCommentsStackAround commentsBeforeBody commentsAfterBody $
                exprParensNone $
                  formatExpr body
        ]
  where
    formatPat (comments, pat) =
      withCommentsBefore comments $
        patternParensProtectSpaces $
          formatPattern (A.toValue pat)

formatTypeAnnotation :: Maybe String -> Name -> Src.Type_ -> Block
formatTypeAnnotation prefix name t =
  spaceOrIndent
    [ Block.line $ withPrefix $ utf8 name <> Block.space <> Block.char7 ':',
      typeParensNone $ formatType t
    ]
  where
    withPrefix a =
      case prefix of
        Nothing -> a
        Just prefixString ->
          Block.string7 prefixString <> Block.char7 ' ' <> a

formatUnion :: Src.Union -> Block
formatUnion (Src.Union name args ctors) =
  Block.stack $
    spaceOrIndent
      [ Block.line (Block.string7 "type"),
        spaceOrIndent $
          Block.line (utf8 $ A.toValue name)
            :| fmap (Block.line . utf8 . A.toValue) args
      ]
      :| fmap Block.indent formatCtors
  where
    formatCtors =
      case ctors of
        [] -> []
        (first : rest) -> formatCtor '=' first : fmap (formatCtor '|') rest

formatCtor :: Char -> (A.Located Name, [([Src.Comment], Src.Type)]) -> Block
formatCtor open (name, args) =
  spaceOrIndent $
    Block.line (Block.char7 open <> Block.space <> utf8 (A.toValue name))
      :| fmap (typeParensProtectSpaces . formatArg) args
  where
    formatArg (comments, arg) =
      formatType (A.toValue arg)

formatAlias :: Src.Alias -> Block
formatAlias (Src.Alias name args type_) =
  Block.stack
    [ spaceOrIndent
        [ Block.line (Block.string7 "type alias"),
          spaceOrIndent $
            Block.line (utf8 $ A.toValue name)
              :| fmap (Block.line . utf8 . A.toValue) args,
          Block.line (Block.char7 '=')
        ],
      Block.indent $ typeParensNone $ formatType (A.toValue type_)
    ]

formatPort :: Src.Port -> Block
formatPort = \case
  Src.Port name type_ ->
    formatTypeAnnotation (Just "port") (A.toValue name) (A.toValue type_)

data ExpressionBlock
  = NoExpressionParens Block
  | ExpressionContainsInfixOps Block
  | ExpressionContainsSpaces Block
  | ExpressionHasAmbiguousEnd Block

-- "no parens"
exprParensNone :: ExpressionBlock -> Block
exprParensNone = \case
  NoExpressionParens block -> block
  ExpressionContainsInfixOps block -> block
  ExpressionContainsSpaces block -> block
  ExpressionHasAmbiguousEnd block -> block

exprParensProtectInfixOps :: ExpressionBlock -> Block
exprParensProtectInfixOps = \case
  NoExpressionParens block -> block
  ExpressionContainsInfixOps block -> parens block
  ExpressionContainsSpaces block -> block
  ExpressionHasAmbiguousEnd block -> parens block

exprParensProtectSpaces :: ExpressionBlock -> Block
exprParensProtectSpaces = \case
  NoExpressionParens block -> block
  ExpressionContainsInfixOps block -> parens block
  ExpressionContainsSpaces block -> parens block
  ExpressionHasAmbiguousEnd block -> parens block

formatExpr :: Src.Expr_ -> ExpressionBlock
formatExpr = \case
  Src.Chr char ->
    NoExpressionParens $
      formatString StringStyleChar char
  Src.Str string ->
    NoExpressionParens $
      formatString StringStyleSingleQuoted string
  Src.Int int ->
    NoExpressionParens $
      Block.line $
        Block.string7 (show int)
  Src.Float float ->
    NoExpressionParens $
      Block.line $
        utf8 float
  Src.Var _ name ->
    NoExpressionParens $
      Block.line $
        utf8 name
  Src.VarQual _ ns name ->
    NoExpressionParens $
      Block.line $
        utf8 ns <> Block.char7 '.' <> utf8 name
  Src.Array exprs ->
    NoExpressionParens $
      group '[' ',' ']' True $
        fmap (exprParensNone . formatExpr . A.toValue) exprs
  Src.Op name ->
    NoExpressionParens $
      Block.line $
        Block.char7 '(' <> utf8 name <> Block.char7 ')'
  Src.Negate expr ->
    NoExpressionParens $
      Block.prefix 1 (Block.char7 '-') $
        exprParensProtectSpaces $
          formatExpr $
            A.toValue expr
  Src.Binops postfixOps final ->
    let (first, rest) = repair3 postfixOps final
     in ExpressionContainsInfixOps $
          spaceOrIndentForce forceMultiline $
            exprParensProtectInfixOps (formatExpr $ A.toValue first)
              :| fmap formatPair rest
    where
      -- for now we just use multiline formatting for specific operators,
      -- since we don't yet track where the linebreaks are in the source
      forceMultiline = any (opForcesMultiline . opFromPair) postfixOps
      opFromPair (_, _, name) = A.toValue name
      formatPair (commentsBeforeOp, op, expr) =
        Block.prefix
          4
          (utf8 (A.toValue op) <> Block.space)
          (exprParensProtectInfixOps $ formatExpr $ A.toValue expr)
  Src.Lambda [] body _ ->
    formatExpr $ A.toValue body
  Src.Lambda (arg1 : args) body (SC.LambdaComments commentsBeforeArrow commentsAfterArrow) ->
    ExpressionHasAmbiguousEnd $
      spaceOrIndent
        [ Block.prefix 1 (Block.char7 '\\') $
            spaceOrStack $
              join
                [ fmap formatArg (arg1 :| args),
                  pure $
                    withCommentsBefore commentsBeforeArrow $
                      Block.line $
                        Block.string7 "->"
                ],
          withCommentsBefore commentsAfterArrow $
            exprParensNone $
              formatExpr $
                A.toValue body
        ]
    where
      formatArg (commentsBefore, arg) =
        withCommentsBefore commentsBefore $
          patternParensProtectSpaces (formatPattern $ A.toValue arg)
  Src.Call fn [] ->
    formatExpr $ A.toValue fn
  Src.Call fn args ->
    ExpressionContainsSpaces $
      spaceOrIndent $
        exprParensProtectInfixOps (formatExpr $ A.toValue fn)
          :| fmap formatArg args
    where
      formatArg (commentsBefore, arg) =
        exprParensProtectSpaces (formatExpr $ A.toValue arg)
  Src.If [] else_ ->
    formatExpr $ A.toValue else_
  Src.If (if_ : elseifs) else_ ->
    ExpressionHasAmbiguousEnd $
      Block.stack $
        NonEmpty.fromList $
          mconcat
            [ List.singleton $ formatIfClause "if" if_,
              fmap (formatIfClause "else if") elseifs,
              List.singleton $
                Block.stack
                  [ Block.line $ Block.string7 "else",
                    Block.indent $ exprParensNone $ formatExpr $ A.toValue else_
                  ]
            ]
    where
      formatIfClause :: String -> (Src.Expr, Src.Expr) -> Block
      formatIfClause keyword (predicate, body) =
        Block.stack
          [ spaceOrStack
              [ spaceOrIndent
                  [ Block.line $ Block.string7 keyword,
                    exprParensNone $ formatExpr $ A.toValue predicate
                  ],
                Block.line $ Block.string7 "then"
              ],
            Block.indent $ exprParensNone $ formatExpr $ A.toValue body
          ]
  Src.Let [] body _ ->
    formatExpr $ A.toValue body
  Src.Let (def1 : defs) body (SC.LetComments commentsBeforeIn commentsAfterIn) ->
    ExpressionHasAmbiguousEnd $
      Block.stack
        [ Block.line (Block.string7 "let"),
          Block.indent $ Block.stack $ NonEmpty.intersperse Block.blankLine $ fmap formatDef (def1 :| defs),
          case formatCommentBlock commentsBeforeIn of
            Nothing -> Block.line (Block.string7 "in")
            Just comments ->
              Block.stack
                [ Block.blankLine,
                  Block.indent comments,
                  Block.line (Block.string7 "in")
                ],
          withCommentsStackBefore commentsAfterIn $ exprParensNone $ formatExpr (A.toValue body)
        ]
  Src.Case subject branches (SC.CaseComments commentsAfterCaseKeyword commentsBeforeOfKeyword) ->
    ExpressionHasAmbiguousEnd $
      Block.stack $
        spaceOrStack
          [ spaceOrIndent
              [ Block.line (Block.string7 "case"),
                withCommentsAround commentsAfterCaseKeyword commentsBeforeOfKeyword $
                  exprParensNone $
                    formatExpr (A.toValue subject)
              ],
            Block.line (Block.string7 "of")
          ]
          :| List.intersperse Block.blankLine (fmap (Block.indent . formatCaseBranch) branches)
    where
      formatCaseBranch (pat, expr, SC.CaseBranchComments commentsBefore commentsAfterPattern commentsBeforeBody commentsAfterBody) =
        withCommentsStackBefore commentsBefore $
          Block.stack
            [ spaceOrStack
                [ withCommentsAround [] commentsAfterPattern $
                    patternParensNone $
                      formatPattern (A.toValue pat),
                  Block.line $ Block.string7 "->"
                ],
              Block.indent $
                withCommentsStackAround commentsBeforeBody commentsAfterBody $
                  exprParensNone $
                    formatExpr $
                      A.toValue expr
            ]
  Src.Accessor field ->
    NoExpressionParens $
      Block.line $
        Block.char7 '.' <> utf8 field
  Src.Access expr field ->
    NoExpressionParens $
      Block.addSuffix (Block.char7 '.' <> utf8 (A.toValue field)) (exprParensProtectSpaces $ formatExpr $ A.toValue expr)
  Src.Update base [] ->
    formatExpr $ A.toValue base
  Src.Update base (first : rest) ->
    NoExpressionParens $
      extendedGroup
        '{'
        '|'
        ','
        '='
        '}'
        (exprParensNone $ formatExpr $ A.toValue base)
        (fmap formatField $ first :| rest)
    where
      formatField (field, expr) =
        ( utf8 $ A.toValue field,
          exprParensNone $ formatExpr (A.toValue expr)
        )
  Src.Record fields ->
    NoExpressionParens $
      group '{' ',' '}' True $
        fmap formatField fields
    where
      formatField (name, expr) =
        spaceOrIndent
          [ Block.line $ utf8 (A.toValue name) <> Block.space <> Block.char7 '=',
            exprParensNone $ formatExpr (A.toValue expr)
          ]
  Src.Parens [] expr [] ->
    formatExpr $ A.toValue expr
  Src.Parens commentsBefore expr commentsAfter ->
    NoExpressionParens $
      parens $
        spaceOrStack $
          NonEmpty.fromList $
            catMaybes
              [ formatCommentBlock commentsBefore,
                Just $ exprParensNone $ formatExpr (A.toValue expr),
                formatCommentBlock commentsAfter
              ]

opForcesMultiline :: Name -> Bool
opForcesMultiline op =
  op == Utf8.fromChars "|>"
    || op == Utf8.fromChars "<|"

formatDef :: ([Src.Comment], A.Located Src.Def) -> Block
formatDef (commentsBefore, def) =
  withCommentsStackBefore commentsBefore $
    case A.toValue def of
      Src.Define name args body ann comments ->
        formatBasicDef (A.toValue name) args (A.toValue body) (fmap A.toValue ann) comments
      Src.Destruct pat body ->
        Block.stack
          [ spaceOrIndent
              [ patternParensProtectSpaces $ formatPattern $ A.toValue pat,
                Block.line $ Block.char7 '='
              ],
            Block.indent $ exprParensNone $ formatExpr $ A.toValue body
          ]

data TypeBlock
  = NoTypeParens Block
  | TypeContainsArrow Block
  | TypeContainsSpaces Block

typeParensNone :: TypeBlock -> Block
typeParensNone = \case
  NoTypeParens block -> block
  TypeContainsArrow block -> block
  TypeContainsSpaces block -> block

typeParensProtectArrows :: TypeBlock -> Block
typeParensProtectArrows = \case
  NoTypeParens block -> block
  TypeContainsArrow block -> parens block
  TypeContainsSpaces block -> block

typeParensProtectSpaces :: TypeBlock -> Block
typeParensProtectSpaces = \case
  NoTypeParens block -> block
  TypeContainsArrow block -> parens block
  TypeContainsSpaces block -> parens block

collectLambdaTypes :: Src.Type -> Src.Type -> NonEmpty (Src.Type)
collectLambdaTypes left = \case
  (A.At _ (Src.TLambda next rest)) ->
    NonEmpty.cons left (collectLambdaTypes next rest)
  other ->
    left :| [other]

formatType :: Src.Type_ -> TypeBlock
formatType = \case
  Src.TLambda left right ->
    TypeContainsArrow $
      case collectLambdaTypes left right of
        (first :| rest) ->
          spaceOrStack $
            (typeParensProtectArrows $ formatType (A.toValue first))
              :| fmap (Block.prefix 3 (Block.string7 "-> ") . typeParensProtectArrows . formatType . A.toValue) rest
  Src.TVar name ->
    NoTypeParens $
      Block.line (utf8 name)
  Src.TType _ name [] ->
    NoTypeParens $
      Block.line (utf8 name)
  Src.TType _ name args ->
    TypeContainsSpaces $
      spaceOrIndent $
        Block.line (utf8 name)
          :| fmap (typeParensProtectSpaces . formatArg) args
    where
      formatArg (comments, arg) =
        formatType (A.toValue arg)
  Src.TTypeQual _ ns name [] ->
    NoTypeParens $
      Block.line (utf8 ns <> Block.char7 '.' <> utf8 name)
  Src.TTypeQual _ ns name args ->
    TypeContainsSpaces $
      spaceOrIndent $
        Block.line (utf8 ns <> Block.char7 '.' <> utf8 name)
          :| fmap (typeParensProtectSpaces . formatArg) args
    where
      formatArg (comments, arg) =
        formatType (A.toValue arg)
  Src.TRecord fields Nothing ->
    NoTypeParens $
      group '{' ',' '}' True $
        fmap formatField fields
    where
      formatField (name, type_) =
        spaceOrIndent
          [ Block.line $ utf8 (A.toValue name) <> Block.space <> Block.char7 ':',
            typeParensNone $ formatType (A.toValue type_)
          ]
  Src.TRecord [] (Just base) ->
    NoTypeParens $
      Block.line $
        utf8 $
          A.toValue base
  Src.TRecord (first : rest) (Just base) ->
    NoTypeParens $
      extendedGroup
        '{'
        '|'
        ','
        ':'
        '}'
        (Block.line $ utf8 $ A.toValue base)
        (fmap formatField $ first :| rest)
    where
      formatField (field, type_) =
        ( utf8 $ A.toValue field,
          typeParensNone $ formatType $ A.toValue type_
        )

data PatternBlock
  = NoPatternParens Block
  | PatternContainsSpaces Block

patternParensNone :: PatternBlock -> Block
patternParensNone = \case
  NoPatternParens block -> block
  PatternContainsSpaces block -> block

patternParensProtectSpaces :: PatternBlock -> Block
patternParensProtectSpaces = \case
  NoPatternParens block -> block
  PatternContainsSpaces block -> parens block

formatPattern :: Src.Pattern_ -> PatternBlock
formatPattern = \case
  Src.PAnything name ->
    NoPatternParens $
      Block.line $
        Block.char7 '_' <> utf8 name
  Src.PVar name ->
    NoPatternParens $
      Block.line $
        utf8 name
  Src.PRecord fields ->
    NoPatternParens $
      group '{' ',' '}' False $
        fmap (formatField . A.toValue) fields
    where
      formatField = \case
        Src.RFPattern (A.At _ name) (A.At _ (Src.PVar pname))
          | name == pname ->
              Block.line $ utf8 name
        Src.RFPattern name pat ->
          spaceOrIndent
            [ Block.line $ utf8 (A.toValue name) <> Block.space <> Block.char7 '=',
              patternParensNone $ formatPattern (A.toValue pat)
            ]
  Src.PAlias pat name ->
    PatternContainsSpaces $
      spaceOrIndent
        [ patternParensProtectSpaces $ formatPattern (A.toValue pat),
          Block.line $ Block.string7 "as " <> utf8 (A.toValue name)
        ]
  Src.PCtor _ name [] ->
    NoPatternParens $
      Block.line (utf8 name)
  Src.PCtor _ name args ->
    PatternContainsSpaces $
      spaceOrIndent $
        Block.line (utf8 name)
          :| fmap (patternParensProtectSpaces . formatPatternConstructorArg) args
  Src.PCtorQual _ ns name [] ->
    NoPatternParens $
      Block.line (utf8 ns <> Block.char7 '.' <> utf8 name)
  Src.PCtorQual _ ns name args ->
    PatternContainsSpaces $
      spaceOrIndent $
        Block.line (utf8 ns <> Block.char7 '.' <> utf8 name)
          :| fmap (patternParensProtectSpaces . formatPatternConstructorArg) args
  Src.PArray items ->
    NoPatternParens $
      group '[' ',' ']' False $
        fmap (patternParensNone . formatPattern . A.toValue) items
  Src.PChr char ->
    NoPatternParens $
      formatString StringStyleChar char
  Src.PStr string ->
    NoPatternParens $
      formatString StringStyleSingleQuoted string
  Src.PInt int ->
    NoPatternParens $
      Block.line $
        Block.string7 (show int)

formatPatternConstructorArg :: ([Src.Comment], Src.Pattern) -> PatternBlock
formatPatternConstructorArg (commentsBefore, pat) =
  formatPattern (A.toValue pat)

data StringStyle
  = StringStyleChar
  | StringStyleSingleQuoted
  | StringStyleTripleQuoted
  deriving (Eq)

formatString :: StringStyle -> Utf8.Utf8 any -> Block
formatString style str =
  case style of
    StringStyleChar ->
      stringBox (Block.char7 '\'')
    StringStyleSingleQuoted ->
      stringBox (Block.char7 '"')
    StringStyleTripleQuoted ->
      stringBox (Block.string7 "\"\"\"")
  where
    stringBox :: Block.Line -> Block
    stringBox quotes =
      Block.line $ quotes <> utf8 str <> quotes
