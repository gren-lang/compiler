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
      -- TODO: eventually all these comments should be retained instead of dropped
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
                           "import Module2WithAs as M2",
                           "import Module3WithExposing exposing (..)",
                           "import Module4WithAsAndExposing as M4 exposing (..)",
                           "",
                           "",
                           "",
                           "{- D -}",
                           formattedModuleBody
                         ]

  describe "top-level definition" $ do
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

    describe "value declarations" $ do
      it "formats comments" $
        ["f{-A-}x{-B-}y{-C-}={-D-}[]"]
          `shouldFormatModuleBodyAs` [ "f {- A -} x {- B -} y {- C -} =",
                                       "    {- D -}",
                                       "    []"
                                     ]
      it "formats indented comments after the body" $ do
        [ "f = []",
          " {-B-}"
          ]
          `shouldFormatModuleBodyAs` [ "f =",
                                       "    []",
                                       "    {- B -}"
                                     ]

  describe "expressions" $ do
    describe "lambda" $ do
      it "formats comments" $
        ["\\{-A-}x{-B-}y{-C-}->{-D-}[]"]
          `shouldFormatExpressionAs` ["\\{- A -} x {- B -} y {- C -} -> {- D -} []"]

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
          "in x"
        ]
          `shouldFormatExpressionAs` [ "let",
                                       "    x =",
                                       "        1",
                                       "        {- A -}",
                                       "in",
                                       "x"
                                     ]

    describe "case" $ do
      it "formats comments" $
        [ "case{-A-}x{-B-}of{-C-}",
          "{-D-}",
          " Nothing{-E-}->{-F-}y",
          "{-H-}",
          " _{-J-}->{-K-}z"
        ]
          `shouldFormatExpressionAs` [ "case {- A -} x {- B -} of",
                                       "    {- C -} {- D -}",
                                       "    Nothing {- E -} ->",
                                       "        {- F -}",
                                       "        y",
                                       "",
                                       "    {- H -}",
                                       "    _ {- J -} ->",
                                       "        {- K -}",
                                       "        z"
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
shouldFormatModuleBodyAs inputLines expectedOutputLines =
  let input = TE.encodeUtf8 $ Text.unlines inputLines
      expectedOutput = LazyText.unlines $ fmap LazyText.fromStrict expectedOutputLines
      actualOutput = LTE.decodeUtf8 . Builder.toLazyByteString <$> Format.formatByteString Parse.Application input
   in case LazyText.stripPrefix "module Main exposing (..)\n\n\n\n" <$> actualOutput of
        Left _ ->
          expectationFailure "shouldFormatModuleBodyAs: failed to format"
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
