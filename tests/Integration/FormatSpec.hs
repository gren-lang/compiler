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
import Test.Hspec

spec :: Spec
spec = do
  describe "module header" $ do
    let formattedModuleBody = "\n\n\nf =\n    {}"
    describe "normal module" $ do
      it "formats already formatted" $
        assertFormatted $
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

  describe "top-level definition" $ do
    it "formats" $
      ["f = {}"]
        `shouldFormatModuleBodyAs` [ "f =",
                                     "    {}"
                                   ]
  describe "expressions" $ do
    describe "record" $ do
      it "formats with fields" $
        ["{a=1,   b = 2}"]
          `shouldFormatExpressionAs` [ "{ a = 1",
                                       ", b = 2",
                                       "}"
                                     ]

assertFormatted :: [Text] -> IO ()
assertFormatted lines_ =
  lines_ `shouldFormatAs` lines_

shouldFormatAs :: [Text] -> [Text] -> IO ()
shouldFormatAs inputLines expectedOutputLines =
  let input = TE.encodeUtf8 $ Text.unlines inputLines
      expectedOutput = LazyText.fromStrict $ Text.unlines expectedOutputLines
      actualOutput = LTE.decodeUtf8 . Builder.toLazyByteString <$> Format.formatByteString input
   in case actualOutput of
        Nothing ->
          expectationFailure "shouldFormatAs: failed to format"
        Just actualModuleBody ->
          actualModuleBody `shouldBe` expectedOutput

shouldFormatModuleBodyAs :: [Text] -> [LazyText.Text] -> IO ()
shouldFormatModuleBodyAs inputLines expectedOutputLines =
  let input = TE.encodeUtf8 $ Text.unlines inputLines
      expectedOutput = LazyText.unlines expectedOutputLines
      actualOutput = LTE.decodeUtf8 . Builder.toLazyByteString <$> Format.formatByteString input
   in case LazyText.stripPrefix "module Main exposing (..)\n\n\n\n" <$> actualOutput of
        Nothing ->
          expectationFailure "shouldFormatModuleBodyAs: failed to format"
        Just Nothing ->
          expectationFailure "shouldFormatModuleBodyAs: internal error: could not strip module header"
        Just (Just actualModuleBody) ->
          actualModuleBody `shouldBe` expectedOutput

shouldFormatExpressionAs :: [Text] -> [LazyText.Text] -> IO ()
shouldFormatExpressionAs inputLines expectedOutputLines =
  let input = TE.encodeUtf8 $ "expr =\n" <> Text.unlines (fmap ("    " <>) inputLines)
      expectedOutput = LazyText.unlines expectedOutputLines
      actualOutput = LTE.decodeUtf8 . Builder.toLazyByteString <$> Format.formatByteString input
      cleanOutput i =
        LazyText.stripPrefix "module Main exposing (..)\n\n\n\nexpr =\n" i
          >>= (return . LazyText.lines)
          >>= traverse (LazyText.stripPrefix "    ")
          >>= (return . LazyText.unlines)
   in case fmap cleanOutput actualOutput of
        Nothing ->
          expectationFailure "shouldFormatExpressionAs: failed to format"
        Just Nothing ->
          expectationFailure "shouldFormatExpressionAs: internal error: could clean output"
        Just (Just actualExpression) ->
          actualExpression `shouldBe` expectedOutput
