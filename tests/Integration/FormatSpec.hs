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
  describe "expressions" $ do
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
        Left _ ->
          expectationFailure "shouldFormatAs: failed to format"
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
          >>= traverse (LazyText.stripPrefix "    ")
          >>= (return . LazyText.unlines)
   in case fmap cleanOutput actualOutput of
        Left _ ->
          expectationFailure "shouldFormatExpressionAs: failed to format"
        Right Nothing ->
          expectationFailure "shouldFormatExpressionAs: internal error: could clean output"
        Right (Just actualExpression) ->
          actualExpression `shouldBe` expectedOutput
