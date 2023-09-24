{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wall #-}
-- Temporary while implementing gren format
{-# OPTIONS_GHC -Wno-error=unused-do-bind #-}

module Gren.Docs
  ( Documentation,
    Module (..),
    fromModule,
    Union (..),
    Alias (..),
    Value (..),
    Binop (..),
    Binop.Associativity (..),
    Binop.Precedence (..),
    Error (..),
    decoder,
    encode,
  )
where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import AST.Utils.Binop qualified as Binop
import Data.Coerce qualified as Coerce
import Data.List qualified as List
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Map.Merge.Strict qualified as Map
import Data.Name qualified as Name
import Data.NonEmptyList qualified as NE
import Data.OneOrMore qualified as OneOrMore
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Gren.Compiler.Type qualified as Type
import Gren.Compiler.Type.Extract qualified as Extract
import Gren.ModuleName qualified as ModuleName
import Json.Decode qualified as D
import Json.Encode ((==>))
import Json.Encode qualified as E
import Json.String qualified as Json
import Parse.Primitives (Col, Row, word1)
import Parse.Primitives qualified as P
import Parse.Space qualified as Space
import Parse.Symbol qualified as Symbol
import Parse.Variable qualified as Var
import Reporting.Annotation qualified as A
import Reporting.Error.Docs qualified as E
import Reporting.Result qualified as Result

-- DOCUMENTATION

type Documentation =
  Map.Map Name.Name Module

data Module = Module
  { _name :: Name.Name,
    _comment :: Comment,
    _unions :: Map.Map Name.Name Union,
    _aliases :: Map.Map Name.Name Alias,
    _values :: Map.Map Name.Name Value,
    _binops :: Map.Map Name.Name Binop
  }

type Comment = Json.String

data Alias = Alias Comment [Name.Name] Type.Type

data Union = Union Comment [Name.Name] [(Name.Name, [Type.Type])]

data Value = Value Comment Type.Type

data Binop = Binop Comment Type.Type Binop.Associativity Binop.Precedence

-- JSON

encode :: Documentation -> E.Value
encode docs =
  let moduleFolder :: Name.Name -> Module -> [(Json.String, E.Value)] -> [(Json.String, E.Value)]
      moduleFolder name modInfo acc =
        (Name.toChars name ==> encodeModule modInfo) : acc
   in E.object $ Map.foldrWithKey moduleFolder [] docs

encodeModule :: Module -> E.Value
encodeModule (Module name comment unions aliases values binops) =
  E.object $
    [ "name" ==> ModuleName.encode name,
      "comment" ==> E.string comment,
      "unions" ==> E.list encodeUnion (Map.toList unions),
      "aliases" ==> E.list encodeAlias (Map.toList aliases),
      "values" ==> E.list encodeValue (Map.toList values),
      "binops" ==> E.list encodeBinop (Map.toList binops)
    ]

data Error
  = BadAssociativity
  | BadModuleName
  | BadType

decoder :: D.Decoder Error Documentation
decoder =
  D.dict moduleNameKeyDecoder moduleDecoder

moduleNameKeyDecoder :: D.KeyDecoder Error ModuleName.Raw
moduleNameKeyDecoder =
  let keyParser =
        P.specialize (\_ _ _ -> BadModuleName) ModuleName.parser
   in D.KeyDecoder keyParser (\_ _ -> BadModuleName)

moduleDecoder :: D.Decoder Error Module
moduleDecoder =
  Module
    <$> D.field "name" moduleNameDecoder
    <*> D.field "comment" D.string
    <*> D.field "unions" (dictDecoder union)
    <*> D.field "aliases" (dictDecoder alias)
    <*> D.field "values" (dictDecoder value)
    <*> D.field "binops" (dictDecoder binop)

dictDecoder :: D.Decoder Error a -> D.Decoder Error (Map.Map Name.Name a)
dictDecoder entryDecoder =
  Map.fromList <$> D.list (named entryDecoder)

named :: D.Decoder Error a -> D.Decoder Error (Name.Name, a)
named entryDecoder =
  (,)
    <$> D.field "name" nameDecoder
    <*> entryDecoder

nameDecoder :: D.Decoder e Name.Name
nameDecoder =
  fmap Coerce.coerce D.string

moduleNameDecoder :: D.Decoder Error ModuleName.Raw
moduleNameDecoder =
  D.mapError (const BadModuleName) ModuleName.decoder

typeDecoder :: D.Decoder Error Type.Type
typeDecoder =
  D.mapError (const BadType) Type.decoder

-- UNION JSON

encodeUnion :: (Name.Name, Union) -> E.Value
encodeUnion (name, Union comment args cases) =
  E.object
    [ "name" ==> E.name name,
      "comment" ==> E.string comment,
      "args" ==> E.list E.name args,
      "cases" ==> E.list encodeCase cases
    ]

union :: D.Decoder Error Union
union =
  Union
    <$> D.field "comment" D.string
    <*> D.field "args" (D.list nameDecoder)
    <*> D.field "cases" (D.list caseDecoder)

encodeCase :: (Name.Name, [Type.Type]) -> E.Value
encodeCase (tag, args) =
  E.list id [E.name tag, E.list Type.encode args]

caseDecoder :: D.Decoder Error (Name.Name, [Type.Type])
caseDecoder =
  D.pair nameDecoder (D.list typeDecoder)

-- ALIAS JSON

encodeAlias :: (Name.Name, Alias) -> E.Value
encodeAlias (name, Alias comment args tipe) =
  E.object
    [ "name" ==> E.name name,
      "comment" ==> E.string comment,
      "args" ==> E.list E.name args,
      "type" ==> Type.encode tipe
    ]

alias :: D.Decoder Error Alias
alias =
  Alias
    <$> D.field "comment" D.string
    <*> D.field "args" (D.list nameDecoder)
    <*> D.field "type" typeDecoder

-- VALUE JSON

encodeValue :: (Name.Name, Value) -> E.Value
encodeValue (name, Value comment tipe) =
  E.object
    [ "name" ==> E.name name,
      "comment" ==> E.string comment,
      "type" ==> Type.encode tipe
    ]

value :: D.Decoder Error Value
value =
  Value
    <$> D.field "comment" D.string
    <*> D.field "type" typeDecoder

-- BINOP JSON

encodeBinop :: (Name.Name, Binop) -> E.Value
encodeBinop (name, Binop comment tipe assoc prec) =
  E.object
    [ "name" ==> E.name name,
      "comment" ==> E.string comment,
      "type" ==> Type.encode tipe,
      "associativity" ==> encodeAssoc assoc,
      "precedence" ==> encodePrec prec
    ]

binop :: D.Decoder Error Binop
binop =
  Binop
    <$> D.field "comment" D.string
    <*> D.field "type" typeDecoder
    <*> D.field "associativity" assocDecoder
    <*> D.field "precedence" precDecoder

-- ASSOCIATIVITY JSON

encodeAssoc :: Binop.Associativity -> E.Value
encodeAssoc assoc =
  case assoc of
    Binop.Left -> E.chars "left"
    Binop.Non -> E.chars "non"
    Binop.Right -> E.chars "right"

assocDecoder :: D.Decoder Error Binop.Associativity
assocDecoder =
  let left = Json.fromChars "left"
      non = Json.fromChars "non"
      right = Json.fromChars "right"
   in do
        str <- D.string
        if
            | str == left -> return Binop.Left
            | str == non -> return Binop.Non
            | str == right -> return Binop.Right
            | otherwise -> D.failure BadAssociativity

-- PRECEDENCE JSON

encodePrec :: Binop.Precedence -> E.Value
encodePrec (Binop.Precedence n) =
  E.int n

precDecoder :: D.Decoder Error Binop.Precedence
precDecoder =
  Binop.Precedence <$> D.int

-- FROM MODULE

fromModule :: Can.Module -> Either E.Error Module
fromModule modul@(Can.Module _ exports docs _ _ _ _ _) =
  case exports of
    Can.ExportEverything region ->
      Left (E.ImplicitExposing region)
    Can.Export exportDict ->
      case docs of
        Src.NoDocs region ->
          Left (E.NoDocs region)
        Src.YesDocs overview comments ->
          do
            names <- parseOverview overview
            checkNames exportDict names
            checkDefs exportDict overview (Map.fromList comments) modul

-- PARSE OVERVIEW

parseOverview :: Src.DocComment -> Either E.Error [A.Located Name.Name]
parseOverview (Src.DocComment snippet) =
  case P.fromSnippet (chompOverview []) E.BadEnd snippet of
    Left err ->
      Left (E.SyntaxProblem err)
    Right names ->
      Right names

type Parser a =
  P.Parser E.SyntaxProblem a

chompOverview :: [A.Located Name.Name] -> Parser [A.Located Name.Name]
chompOverview names =
  do
    isDocs <- chompUntilDocs
    if isDocs
      then do
        Space.chomp E.Space
        chompOverview =<< chompDocs names
      else return names

chompDocs :: [A.Located Name.Name] -> Parser [A.Located Name.Name]
chompDocs names =
  do
    name <-
      P.addLocation $
        P.oneOf
          E.Name
          [ Var.lower E.Name,
            Var.upper E.Name,
            chompOperator
          ]

    Space.chomp E.Space

    P.oneOfWithFallback
      [ do
          pos <- P.getPosition
          Space.checkIndent pos E.Comma
          word1 0x2C {-,-} E.Comma
          Space.chomp E.Space
          chompDocs (name : names)
      ]
      (name : names)

chompOperator :: Parser Name.Name
chompOperator =
  do
    word1 0x28 {-(-} E.Op
    op <- Symbol.operator E.Op E.OpBad
    word1 0x29 {-)-} E.Op
    return op

chompUntilDocs :: Parser Bool
chompUntilDocs =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ _ ->
    let (# isDocs, newPos, newRow, newCol #) = untilDocs pos end row col
        !newState = P.State src newPos end indent newRow newCol
     in cok isDocs newState

untilDocs :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> (# Bool, Ptr Word8, Row, Col #)
untilDocs pos end row col =
  if pos >= end
    then (# False, pos, row, col #)
    else
      let !word = P.unsafeIndex pos
       in if word == 0x0A {-\n-}
            then untilDocs (plusPtr pos 1) end (row + 1) 1
            else
              let !pos5 = plusPtr pos 5
               in if pos5 <= end
                    && P.unsafeIndex (pos) == 0x40 {-@-}
                    && P.unsafeIndex (plusPtr pos 1) == 0x64 {-d-}
                    && P.unsafeIndex (plusPtr pos 2) == 0x6F {-o-}
                    && P.unsafeIndex (plusPtr pos 3) == 0x63 {-c-}
                    && P.unsafeIndex (plusPtr pos 4) == 0x73 {-s-}
                    && Var.getInnerWidth pos5 end == 0
                    then (# True, pos5, row, col + 5 #)
                    else
                      let !newPos = plusPtr pos (P.getCharWidth word)
                       in untilDocs newPos end row (col + 1)

-- CHECK NAMES

checkNames :: Map.Map Name.Name (A.Located Can.Export) -> [A.Located Name.Name] -> Either E.Error ()
checkNames exports names =
  let docs = List.foldl' addName Map.empty names
      loneDoc = Map.traverseMissing onlyInDocs
      loneExport = Map.traverseMissing onlyInExports
      checkBoth = Map.zipWithAMatched (\n _ r -> isUnique n r)
   in case Result.run (Map.mergeA loneExport loneDoc checkBoth exports docs) of
        (_, Right _) -> Right ()
        (_, Left es) -> Left (E.NameProblems (OneOrMore.destruct NE.List es))

type DocNameRegions =
  Map.Map Name.Name (OneOrMore.OneOrMore A.Region)

addName :: DocNameRegions -> A.Located Name.Name -> DocNameRegions
addName dict (A.At region name) =
  Map.insertWith OneOrMore.more name (OneOrMore.one region) dict

isUnique :: Name.Name -> OneOrMore.OneOrMore A.Region -> Result.Result i w E.NameProblem A.Region
isUnique name regions =
  case regions of
    OneOrMore.One region ->
      Result.ok region
    OneOrMore.More left right ->
      let (r1, r2) = OneOrMore.getFirstTwo left right
       in Result.throw (E.NameDuplicate name r1 r2)

onlyInDocs :: Name.Name -> OneOrMore.OneOrMore A.Region -> Result.Result i w E.NameProblem a
onlyInDocs name regions =
  do
    region <- isUnique name regions
    Result.throw $ E.NameOnlyInDocs name region

onlyInExports :: Name.Name -> A.Located Can.Export -> Result.Result i w E.NameProblem a
onlyInExports name (A.At region _) =
  Result.throw $ E.NameOnlyInExports name region

-- CHECK DEFS

checkDefs :: Map.Map Name.Name (A.Located Can.Export) -> Src.DocComment -> Map.Map Name.Name Src.DocComment -> Can.Module -> Either E.Error Module
checkDefs exportDict overview comments (Can.Module name _ _ decls unions aliases infixes effects) =
  let types = gatherTypes decls Map.empty
      info = Info comments types unions aliases infixes effects
   in case Result.run (Map.traverseWithKey (checkExport info) exportDict) of
        (_, Left problems) -> Left $ E.DefProblems (OneOrMore.destruct NE.List problems)
        (_, Right inserters) -> Right $ foldr ($) (emptyModule name overview) inserters

emptyModule :: ModuleName.Canonical -> Src.DocComment -> Module
emptyModule (ModuleName.Canonical _ name) (Src.DocComment overview) =
  Module name (Json.fromComment overview) Map.empty Map.empty Map.empty Map.empty

data Info = Info
  { _iComments :: Map.Map Name.Name Src.DocComment,
    _iValues :: Map.Map Name.Name (Either A.Region Can.Type),
    _iUnions :: Map.Map Name.Name Can.Union,
    _iAliases :: Map.Map Name.Name Can.Alias,
    _iBinops :: Map.Map Name.Name Can.Binop,
    _iEffects :: Can.Effects
  }

checkExport :: Info -> Name.Name -> A.Located Can.Export -> Result.Result i w E.DefProblem (Module -> Module)
checkExport info name (A.At region export) =
  case export of
    Can.ExportValue ->
      do
        tipe <- getType name info
        comment <- getComment region name info
        Result.ok $ \m ->
          m {_values = Map.insert name (Value comment tipe) (_values m)}
    Can.ExportBinop ->
      do
        let (Can.Binop_ assoc prec realName) = _iBinops info ! name
        tipe <- getType realName info
        comment <- getComment region realName info
        Result.ok $ \m ->
          m {_binops = Map.insert name (Binop comment tipe assoc prec) (_binops m)}
    Can.ExportAlias ->
      do
        let (Can.Alias tvars tipe) = _iAliases info ! name
        comment <- getComment region name info
        Result.ok $ \m ->
          m {_aliases = Map.insert name (Alias comment tvars (Extract.fromType tipe)) (_aliases m)}
    Can.ExportUnionOpen ->
      do
        let (Can.Union tvars ctors _ _) = _iUnions info ! name
        comment <- getComment region name info
        Result.ok $ \m ->
          m {_unions = Map.insert name (Union comment tvars (map dector ctors)) (_unions m)}
    Can.ExportUnionClosed ->
      do
        let (Can.Union tvars _ _ _) = _iUnions info ! name
        comment <- getComment region name info
        Result.ok $ \m ->
          m {_unions = Map.insert name (Union comment tvars []) (_unions m)}
    Can.ExportPort ->
      do
        tipe <- getType name info
        comment <- getComment region name info
        Result.ok $ \m ->
          m {_values = Map.insert name (Value comment tipe) (_values m)}

getComment :: A.Region -> Name.Name -> Info -> Result.Result i w E.DefProblem Comment
getComment region name info =
  case Map.lookup name (_iComments info) of
    Nothing ->
      Result.throw (E.NoComment name region)
    Just (Src.DocComment snippet) ->
      Result.ok (Json.fromComment snippet)

getType :: Name.Name -> Info -> Result.Result i w E.DefProblem Type.Type
getType name info =
  case _iValues info ! name of
    Left region ->
      Result.throw (E.NoAnnotation name region)
    Right tipe ->
      Result.ok (Extract.fromType tipe)

dector :: Can.Ctor -> (Name.Name, [Type.Type])
dector (Can.Ctor name _ _ args) =
  (name, map Extract.fromType args)

-- GATHER TYPES

type Types =
  Map.Map Name.Name (Either A.Region Can.Type)

gatherTypes :: Can.Decls -> Types -> Types
gatherTypes decls types =
  case decls of
    Can.Declare def subDecls ->
      gatherTypes subDecls (addDef types def)
    Can.DeclareRec def defs subDecls ->
      gatherTypes subDecls (List.foldl' addDef (addDef types def) defs)
    Can.SaveTheEnvironment ->
      types

addDef :: Types -> Can.Def -> Types
addDef types def =
  case def of
    Can.Def (A.At region name) _ _ ->
      Map.insert name (Left region) types
    Can.TypedDef (A.At _ name) _ typedArgs _ resultType ->
      let tipe = foldr Can.TLambda resultType (map snd typedArgs)
       in Map.insert name (Right tipe) types
