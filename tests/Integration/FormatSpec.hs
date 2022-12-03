{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for formatting that cover both the parsing and formatting
-- (tests go from text to text).
module Integration.FormatSpec where

import Data.ByteString.Builder qualified as Builder
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LTE
import Format qualified
import Gren.Package qualified
import Parse.Module qualified as Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "module header" $ do
    let formattedModuleBody = "\n\n\nf =\n    {}"
    describe "normal module" $
      do
        it "formats already formatted" $
          assertFormatted
            [ "module Normal exposing (..)",
              formattedModuleBody
            ]
        it "formats" $
          [ "module ",
            " Normal ",
            " exposing ",
            " ( ",
            " .. ",
            " )  ",
            "  ",
            "",
            formattedModuleBody
          ]
            `shouldFormatAs` [ "module Normal exposing (..)",
                               formattedModuleBody
                             ]
        it "formats with comments" $
          [ "{-A-}",
            "module{-B-}Normal{-C-}exposing{-D-}({-E-}..{-F-}){-G-}",
            "{-H-}",
            formattedModuleBody
          ]
            `shouldFormatAs` [ "{- A -}",
                               "module {- B -} Normal {- C -} exposing {- D -} (..)",
                               "{- G -} {- H -}",
                               formattedModuleBody
                             ]

  describe "imports" $ do
    let formattedModuleHeader = "module M exposing (..)\n"
    let formattedModuleBody = "\n\nf =\n    {}"

    it "formats already formatted" $
      assertFormatted
        [ formattedModuleHeader,
          "import APlainImport",
          "import BNamespace.QualifiedImport",
          "import CAliasImport as C",
          "import DExposingImport exposing (..)",
          "import EAliasAndExposing as E exposing (..)",
          formattedModuleBody
        ]

    it "sorts imports by name" $
      [ formattedModuleHeader,
        "import A.B",
        "import A",
        "import C as Z",
        "import B",
        formattedModuleBody
      ]
        `shouldFormatAs` [ formattedModuleHeader,
                           "import A",
                           "import A.B",
                           "import B",
                           "import C as Z",
                           formattedModuleBody
                         ]

    it "formats comments" $
      [ formattedModuleHeader,
        "import{-A-}Module1{-B-}",
        " {-C-}",
        "import{-D-}Module2{-E-}as{-F-}M2{-G-}",
        " {-H-}",
        "import{-I-}Module3{-J-}exposing{-K-}(..){-L-}",
        " {-M-}",
        "import{-N-}Module4{-O-}as{-P-}M4{-Q-}exposing{-R-}(..){-S-}",
        " {-T-}",
        formattedModuleBody
      ]
        `shouldFormatAs` [ formattedModuleHeader,
                           "import {- A -} Module1 {- B -} {- C -}",
                           "import {- D -} Module2 {- E -} as {- F -} M2 {- G -} {- H -}",
                           "import {- I -} Module3 {- J -} exposing {- K -} (..) {- L -} {- M -}",
                           "import {- N -} Module4 {- O -} as {- P -} M4 {- Q -} exposing {- R -} (..) {- S -} {- T -}",
                           formattedModuleBody
                         ]
    it "does not attach unindented comments to the import line" $
      [ formattedModuleHeader,
        "import Module1",
        "{-A-}",
        "import Module2WithAs as M2",
        "{-B-}",
        "import Module3WithExposing exposing (..)",
        "{-C-}",
        "import Module4WithAsAndExposing as M4 exposing (..)",
        "{-D-}",
        formattedModuleBody
      ]
        `shouldFormatAs` [ formattedModuleHeader,
                           "import Module1",
                           "",
                           "{- A -}",
                           "import Module2WithAs as M2",
                           "",
                           "{- B -}",
                           "import Module3WithExposing exposing (..)",
                           "",
                           "{- C -}",
                           "import Module4WithAsAndExposing as M4 exposing (..)",
                           "",
                           "",
                           "",
                           "{- D -}",
                           formattedModuleBody
                         ]

    describe "import listings" $ do
      it "formats" $
        assertFormatted
          [ formattedModuleHeader,
            "import Module1 exposing ( (+), f, T1, T2(..), T3 )",
            formattedModuleBody
          ]

  describe "top-level definitions" $ do
    it "formats already formatted" $
      assertFormattedModuleBody
        [ "f x =",
          "    {}"
        ]
    it "formats" $
      ["f = {}"]
        `shouldFormatModuleBodyAs` [ "f =",
                                     "    {}"
                                   ]
    it "formats comments between top-level definitions" $
      [ "f = {}",
        "-- B",
        "g = {}",
        "-- C",
        "h = {}"
      ]
        `shouldFormatModuleBodyAs` [ "f =",
                                     "    {}",
                                     "",
                                     "",
                                     "",
                                     "-- B",
                                     "",
                                     "",
                                     "g =",
                                     "    {}",
                                     "",
                                     "",
                                     "",
                                     "-- C",
                                     "",
                                     "",
                                     "h =",
                                     "    {}"
                                   ]
    describe "formats comments between imports and first declaration" $ do
      it "basic last import" $
        [ "module Main exposing (..)",
          "import Html",
          "-- A",
          "f = {}"
        ]
          `shouldFormatAs` [ "module Main exposing (..)",
                             "",
                             "import Html",
                             "",
                             "",
                             "",
                             "-- A",
                             "",
                             "",
                             "f =",
                             "    {}"
                           ]
      it "last import has 'as' clause" $
        [ "module Main exposing (..)",
          "import Html as H",
          "-- A",
          "f = {}"
        ]
          `shouldFormatAs` [ "module Main exposing (..)",
                             "",
                             "import Html as H",
                             "",
                             "",
                             "",
                             "-- A",
                             "",
                             "",
                             "f =",
                             "    {}"
                           ]
      it "last import has 'exposing' clause" $
        [ "module Main exposing (..)",
          "import Html exposing (div)",
          "-- A",
          "f = {}"
        ]
          `shouldFormatAs` [ "module Main exposing (..)",
                             "",
                             "import Html exposing ( div )",
                             "",
                             "",
                             "",
                             "-- A",
                             "",
                             "",
                             "f =",
                             "    {}"
                           ]
      it "last import has 'as' clause and `exposing` clause" $
        [ "module Main exposing (..)",
          "import Html as H exposing (div)",
          "-- A",
          "f = {}"
        ]
          `shouldFormatAs` [ "module Main exposing (..)",
                             "",
                             "import Html as H exposing ( div )",
                             "",
                             "",
                             "",
                             "-- A",
                             "",
                             "",
                             "f =",
                             "    {}"
                           ]
    it "formats comments after custom type declarations" $
      [ "type T1 = T1a | T1b",
        "-- A",
        "f = {}"
      ]
        `shouldFormatModuleBodyAs` [ "type T1",
                                     "    = T1a",
                                     "    | T1b",
                                     "",
                                     "",
                                     "",
                                     "-- A",
                                     "",
                                     "",
                                     "f =",
                                     "    {}"
                                   ]
    it "formats comments after value declarations ending in a case expression" $
      [ "f =",
        "    case x of",
        "        _ -> {}",
        "-- A",
        "g = {}"
      ]
        `shouldFormatModuleBodyAs` [ "f =",
                                     "    case x of",
                                     "        _ ->",
                                     "            {}",
                                     "",
                                     "",
                                     "",
                                     "-- A",
                                     "",
                                     "",
                                     "g =",
                                     "    {}"
                                   ]

    describe "union type declarations" $ do
      it "formats comments" $
        ["type{-A-}T{-B-}a{-C-}b{-D-}={-E-}A{-F-}a{-G-}a{-H-}|{-I-}B{-J-}b"]
          `shouldFormatModuleBodyAs` [ "type {- A -} T {- B -} a {- C -} b {- D -}",
                                       "    = {- E -} A {- F -} a {- G -} a {- H -}",
                                       "    | {- I -} B {- J -} b"
                                     ]
      it "formats indented comments after last variant" $
        [ "type T = A{-A-}",
          " {-B-}",
          "{-C-}"
        ]
          `shouldFormatModuleBodyAs` [ "type T",
                                       "    = A {- A -} {- B -}",
                                       "",
                                       "",
                                       "",
                                       "{- C -}"
                                     ]

    describe "value declarations" $ do
      it "formats comments" $
        ["f{-A-}x{-B-}y{-C-}={-D-}[]"]
          `shouldFormatModuleBodyAs` [ "f {- A -} x {- B -} y {- C -} =",
                                       "    {- D -}",
                                       "    []"
                                     ]
      it "formats indented comments after the body" $
        [ "f = []",
          " {-B-}"
        ]
          `shouldFormatModuleBodyAs` [ "f =",
                                       "    []",
                                       "    {- B -}"
                                     ]
      it "formats comments in type annotations" $
        [ "f{-A-}:{-B-}Int{-C-}",
          "f =",
          "    0"
        ]
          `shouldFormatModuleBodyAs` [ "f {- A -} : {- B -} Int {- C -}",
                                       "f =",
                                       "    0"
                                     ]
    describe "operator declarations" $ do
      it "formats" $
        [ "infix left 0 (|>) = apR",
          "infix right 0 (<|) = apL",
          "f = {}"
        ]
          `shouldFormatKernelModuleBodyAs` [ "infix left  0 (|>) = apR",
                                             "infix right 0 (<|) = apL",
                                             "",
                                             "",
                                             "f =",
                                             "    {}"
                                           ]

  describe "expressions" $ do
    describe "array literals" $ do
      it "formats" $
        ["[1,2,3]"]
          `shouldFormatExpressionAs` [ "[ 1",
                                       ", 2",
                                       ", 3",
                                       "]"
                                     ]

      it "formats comments" $
        ["[{-A-}1{-B-},{-C-}2{-D-},{-E-}3{-F-}]"]
          `shouldFormatExpressionAs` [ "[ {- A -}",
                                       "  1",
                                       "    {- B -}",
                                       "",
                                       ", {- C -}",
                                       "  2",
                                       "    {- D -}",
                                       "",
                                       ", {- E -}",
                                       "  3",
                                       "    {- F -}",
                                       "]"
                                     ]

    describe "unary operators (negation)" $ do
      it "formats" $
        ["-x"]
          `shouldFormatExpressionAs` ["-x"]
      it "formats multiline value" $
        [ "-(x --A",
          ")"
        ]
          `shouldFormatExpressionAs` [ "-(x",
                                       "  -- A",
                                       " )"
                                     ]

    describe "binary operators" $ do
      it "formats" $
        ["1+2*3"]
          `shouldFormatExpressionAs` ["1 + 2 * 3"]

      it "formats comments" $
        ["1{-A-}+{-B-}2{-C-}*{-D-}3"]
          `shouldFormatExpressionAs` ["1 {- A -} + {- B -} 2 {- C -} * {- D -} 3"]

    describe "lambda" $ do
      it "formats comments" $
        ["\\{-A-}x{-B-}y{-C-}->{-D-}[]"]
          `shouldFormatExpressionAs` ["\\{- A -} x {- B -} y {- C -} -> {- D -} []"]

    describe "function call" $ do
      it "formats comments" $
        ["f{-A-}x{-B-}y{-C-}z"]
          `shouldFormatExpressionAs` ["f {- A -} x {- B -} y {- C -} z"]

    describe "if" $ do
      it "formats comments" $
        ["if{-A-}x{-B-}then{-C-}1{-D-}else{-E-}if{-F-}y{-G-}then{-H-}2{-I-}else{-J-}3"]
          `shouldFormatExpressionAs` [ "if {- A -} x {- B -} then",
                                       "    {- C -}",
                                       "    1",
                                       "    {- D -}",
                                       "else if {- E -} {- F -} y {- G -} then",
                                       "    {- H -}",
                                       "    2",
                                       "    {- I -}",
                                       "else",
                                       "    {- J -}",
                                       "    3"
                                     ]
      it "formats indented comments after else body" $
        [ "if x then 1",
          "else 2{-A-}",
          " {-B-}"
        ]
          `shouldFormatExpressionAs` [ "if x then",
                                       "    1",
                                       "else",
                                       "    2",
                                       "    {- A -} {- B -}"
                                     ]

    describe "let" $ do
      it "formats comments" $
        ["let{-A-}x{-D-}={-E-}1{-B-}in{-C-}x"]
          `shouldFormatExpressionAs` [ "let",
                                       "    {- A -}",
                                       "    x {- D -} =",
                                       "        {- E -}",
                                       "        1",
                                       "        {- B -}",
                                       "in",
                                       "{- C -}",
                                       "x"
                                     ]
      it "formats comments in type annotations" $
        [ "let f{-A-}:{-B-}Int{-C-}",
          "    f =",
          "        0",
          "in x"
        ]
          `shouldFormatExpressionAs` [ "let",
                                       "    f {- A -} : {- B -} Int {- C -}",
                                       "    f =",
                                       "        0",
                                       "in",
                                       "x"
                                     ]
      it "formats comments in destructure declarations" $
        ["let{ x, y }{-A-}={-B-}r in x"]
          `shouldFormatExpressionAs` [ "let",
                                       "    { x, y } {- A -} =",
                                       "        {- B -}",
                                       "        r",
                                       "in",
                                       "x"
                                     ]
      it "formats comments between and after declarations" $
        [ "let",
          "    x = 1",
          "    {-A-}",
          "{-B-}",
          "    y = 2",
          "    {-C-}",
          "{-D-}",
          "in x"
        ]
          `shouldFormatExpressionAs` [ "let",
                                       "    x =",
                                       "        1",
                                       "",
                                       "    {- A -} {- B -}",
                                       "    y =",
                                       "        2",
                                       "",
                                       "    {- C -} {- D -}",
                                       "in",
                                       "x"
                                     ]
      it "formats indented comments after declarations" $
        [ "let",
          "    x = 1",
          "     {-A-}",
          "    {y,z} = r",
          "     {-B-}",
          "in x"
        ]
          `shouldFormatExpressionAs` [ "let",
                                       "    x =",
                                       "        1",
                                       "        {- A -}",
                                       "",
                                       "    { y, z } =",
                                       "        r",
                                       "        {- B -}",
                                       "in",
                                       "x"
                                     ]

    describe "case" $ do
      it "formats comments" $
        [ "case{-A-}x{-B-}of{-C-}",
          " {-D1-}",
          "{-D2-}",
          " Nothing{-E-}->{-F-}y",
          " {-H1-}",
          "{-H2-}",
          " _{-J-}->{-K-}z"
        ]
          `shouldFormatExpressionAs` [ "case {- A -} x {- B -} of",
                                       "    {- C -} {- D1 -} {- D2 -}",
                                       "    Nothing {- E -} ->",
                                       "        {- F -}",
                                       "        y",
                                       "",
                                       "    {- H1 -} {- H2 -}",
                                       "    _ {- J -} ->",
                                       "        {- K -}",
                                       "        z"
                                     ]
      it "formats indented comments after branches" $
        [ "case x of",
          " Nothing -> y{-A-}",
          "  {-B-}",
          " _ -> z{-C-}",
          "  {-D-}"
        ]
          `shouldFormatExpressionAs` [ "case x of",
                                       "    Nothing ->",
                                       "        y",
                                       "        {- A -} {- B -}",
                                       "",
                                       "    _ ->",
                                       "        z",
                                       "        {- C -} {- D -}"
                                     ]

    describe "record" $ do
      describe "empty" $ do
        it "formats already formatted" $
          assertFormattedExpression
            ["{}"]
        it "formats" $
          [ "{",
            " }"
          ]
            `shouldFormatExpressionAs` ["{}"]
      it "formats with fields" $
        ["{a=1,   b = 2}"]
          `shouldFormatExpressionAs` [ "{ a = 1",
                                       ", b = 2",
                                       "}"
                                     ]
      it "formats field with multiline value" $
        [ "{a = --X",
          "1}"
        ]
          `shouldFormatExpressionAs` [ "{ a =",
                                       "    -- X",
                                       "    1",
                                       "}"
                                     ]
      it "formats comments" $
        ["{{-A-}a{-B-}={-C-}1{-D-},{-E-}b{-F-}={-G-}2{-H-}}"]
          `shouldFormatExpressionAs` [ "{ {- A -}",
                                       "  a {- B -} = {- C -} 1 {- D -}",
                                       "",
                                       ", {- E -}",
                                       "  b {- F -} = {- G -} 2 {- H -}",
                                       "}"
                                     ]

    describe "record update" $ do
      it "formats" $
        ["{base|a=1,b=2}"]
          `shouldFormatExpressionAs` [ "{ base",
                                       "    | a = 1",
                                       "    , b = 2",
                                       "}"
                                     ]
      it "formats field with multiline value" $
        [ "{base|a = --X",
          "1}"
        ]
          `shouldFormatExpressionAs` [ "{ base",
                                       "    | a =",
                                       "        -- X",
                                       "        1",
                                       "}"
                                     ]
      it "formats with comments" $
        ["{{-A-}base{-B-}|{-C-}a{-D-}={-E-}1{-F-},{-G-}b{-H-}={-I-}2{-J-}}"]
          `shouldFormatExpressionAs` [ "{ {- A -}",
                                       "  base",
                                       "  {- B -}",
                                       "    | {- C -}",
                                       "      a {- D -} = {- E -} 1 {- F -}",
                                       "",
                                       "    , {- G -}",
                                       "      b {- H -} = {- I -} 2 {- J -}",
                                       "}"
                                     ]

  describe "parentheses" $ do
    it "removes unnecessary parentheses" $
      ["(a)"]
        `shouldFormatExpressionAs` ["a"]
    describe "retains necessary parentheses" $ do
      it "protects nested function application" $
        assertFormattedExpression
          ["f (g x)"]
      it "retains parentheses used to group comments" $
        assertFormattedExpression
          ["({- A -} x)"]

  describe "patterns" $ do
    describe "array patterns" $ do
      it "formats comments" $
        ["f [{-A-}1{-B-},{-C-}2{-D-}] = {}"]
          `shouldFormatModuleBodyAs` [ "f [ {- A -} 1 {- B -}, {- C -} 2 {- D -} ] =",
                                       "    {}"
                                     ]

  describe "types" $ do
    describe "function types" $ do
      it "formats" $
        ["a->b->c->d"]
          `shouldFormatTypeAs` ["a -> b -> c -> d"]
      it "formats comments" $
        ["a{-A-}->{-B-}b"]
          `shouldFormatTypeAs` ["a {- A -} -> {- B -} b"]

    describe "type with arguments" $ do
      it "formats comments" $
        ["Dict{-A-}Int{-B-}String"]
          `shouldFormatTypeAs` ["Dict {- A -} Int {- B -} String"]
      it "formats comments for qualified types" $
        ["Dict.Dict{-A-}Int{-B-}String"]
          `shouldFormatTypeAs` ["Dict.Dict {- A -} Int {- B -} String"]

    describe "record types" $ do
      it "formats with fields" $
        ["{a:Bool,   b : Int}"]
          `shouldFormatTypeAs` [ "{ a : Bool",
                                 ", b : Int",
                                 "}"
                               ]
      it "formats comments" $
        ["{{-A-}a{-B-}:{-C-}Bool{-D-},{-E-}b{-F-}:{-G-}Int{-H-}}"]
          `shouldFormatTypeAs` [ "{ {- A -}",
                                 "  a {- B -} : {- C -} Bool {- D -}",
                                 "",
                                 ", {- E -}",
                                 "  b {- F -} : {- G -} Int {- H -}",
                                 "}"
                               ]
    describe "record extension types" $ do
      it "formats" $
        ["{base|a:Bool,b:Int}"]
          `shouldFormatTypeAs` [ "{ base",
                                 "    | a : Bool",
                                 "    , b : Int",
                                 "}"
                               ]
      it "formats with comments" $
        ["{{-A-}base{-B-}|{-C-}a{-D-}:{-E-}Bool{-F-},{-G-}b{-H-}:{-I-}Int{-J-}}"]
          `shouldFormatTypeAs` [ "{ {- A -}",
                                 "  base",
                                 "  {- B -}",
                                 "    | {- C -}",
                                 "      a {- D -} : {- E -} Bool {- F -}",
                                 "",
                                 "    , {- G -}",
                                 "      b {- H -} : {- I -} Int {- J -}",
                                 "}"
                               ]
    describe "parentheses" $ do
      it "removes unnecessary parens" $
        ["((a -> b)) -> c"]
          `shouldFormatTypeAs` ["(a -> b) -> c"]
      it "formats comments" $
        ["({-A-}Int{-B-})"]
          `shouldFormatTypeAs` ["({- A -} Int {- B -})"]

assertFormatted :: [Text] -> IO ()
assertFormatted lines_ =
  lines_ `shouldFormatAs` lines_

shouldFormatAs :: [Text] -> [Text] -> IO ()
shouldFormatAs inputLines expectedOutputLines =
  let input = TE.encodeUtf8 $ Text.unlines inputLines
      expectedOutput = LazyText.unlines $ fmap LazyText.fromStrict expectedOutputLines
      actualOutput = LTE.decodeUtf8 . Builder.toLazyByteString <$> Format.formatByteString Parse.Application input
   in case actualOutput of
        Left err ->
          expectationFailure ("shouldFormatAs: failed to format: " <> show err)
        Right actualModuleBody ->
          actualModuleBody `shouldBe` expectedOutput

assertFormattedModuleBody :: [Text] -> IO ()
assertFormattedModuleBody lines_ =
  lines_ `shouldFormatModuleBodyAs` lines_

shouldFormatModuleBodyAs :: [Text] -> [Text] -> IO ()
shouldFormatModuleBodyAs =
  shouldFormatModuleBodyAs_ Parse.Application

shouldFormatKernelModuleBodyAs :: [Text] -> [Text] -> IO ()
shouldFormatKernelModuleBodyAs =
  shouldFormatModuleBodyAs_ (Parse.Package Gren.Package.kernel)

shouldFormatModuleBodyAs_ :: Parse.ProjectType -> [Text] -> [Text] -> IO ()
shouldFormatModuleBodyAs_ projectType inputLines expectedOutputLines =
  let input = TE.encodeUtf8 $ Text.unlines inputLines
      expectedOutput = LazyText.unlines $ fmap LazyText.fromStrict expectedOutputLines
      actualOutput = LTE.decodeUtf8 . Builder.toLazyByteString <$> Format.formatByteString projectType input
   in case LazyText.stripPrefix "module Main exposing (..)\n\n\n\n" <$> actualOutput of
        Left err ->
          expectationFailure ("shouldFormatModuleBodyAs: failed to format" <> show err)
        Right Nothing ->
          expectationFailure "shouldFormatModuleBodyAs: internal error: could not strip module header"
        Right (Just actualModuleBody) ->
          actualModuleBody `shouldBe` expectedOutput

assertFormattedExpression :: [Text] -> IO ()
assertFormattedExpression lines_ =
  lines_ `shouldFormatExpressionAs` lines_

shouldFormatExpressionAs :: [Text] -> [Text] -> IO ()
shouldFormatExpressionAs inputLines expectedOutputLines =
  let input = TE.encodeUtf8 $ "expr =\n" <> Text.unlines (fmap ("    " <>) inputLines)
      expectedOutput = LazyText.unlines $ fmap LazyText.fromStrict expectedOutputLines
      actualOutput = LTE.decodeUtf8 . Builder.toLazyByteString <$> Format.formatByteString Parse.Application input
      cleanOutput i =
        LazyText.stripPrefix "module Main exposing (..)\n\n\n\nexpr =\n" i
          >>= (return . LazyText.lines)
          >>= traverse stripIndent
          >>= (return . LazyText.unlines)
   in case fmap cleanOutput actualOutput of
        Left err ->
          expectationFailure ("shouldFormatExpressionAs: failed to format: " <> show err)
        Right Nothing ->
          expectationFailure ("shouldFormatExpressionAs: internal error: couldn't clean output: " <> show actualOutput)
        Right (Just actualExpression) ->
          actualExpression `shouldBe` expectedOutput
  where
    stripIndent text =
      if text == ""
        then Just text
        else LazyText.stripPrefix "    " text

shouldFormatTypeAs :: [Text] -> [Text] -> IO ()
shouldFormatTypeAs inputLines expectedOutputLines =
  let input = TE.encodeUtf8 $ "type alias Type =\n" <> Text.unlines (fmap ("    " <>) inputLines)
      expectedOutput = LazyText.unlines $ fmap LazyText.fromStrict expectedOutputLines
      actualOutput = LTE.decodeUtf8 . Builder.toLazyByteString <$> Format.formatByteString Parse.Application input
      cleanOutput i =
        LazyText.stripPrefix "module Main exposing (..)\n\n\n\ntype alias Type =\n" i
          >>= (return . LazyText.lines)
          >>= traverse stripIndent
          >>= (return . LazyText.unlines)
   in case fmap cleanOutput actualOutput of
        Left err ->
          expectationFailure ("shouldFormatTypeAs: failed to format: " <> show err)
        Right Nothing ->
          expectationFailure ("shouldFormatTypeAs: internal error: couldn't clean output: " <> show actualOutput)
        Right (Just actualExpression) ->
          actualExpression `shouldBe` expectedOutput
  where
    stripIndent text =
      if text == ""
        then Just text
        else LazyText.stripPrefix "    " text
