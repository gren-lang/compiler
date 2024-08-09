{-# LANGUAGE OverloadedStrings #-}

module Parse.MultilineStringSpec where

import AST.Source qualified as Src
import Data.ByteString qualified as BS
import Data.Utf8 qualified as Utf8
import Helpers.Instances ()
import Helpers.Parse qualified as Helpers
import Parse.Expression qualified as Expression
import Parse.Pattern qualified as Pattern
import Reporting.Error.Syntax (Expr (ExpressionBadEnd))
import Reporting.Error.Syntax qualified as Error.Syntax
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  describe "Multiline String" $ do
    it "regression test" $
      parse
        "normal string"
        "\"\"\"\nnormal string\"\"\""

    it "crlf regression test" $ do
      parse
        "normal string"
        "\"\"\"\r\nnormal string\"\"\""

    it "no ending newline works" $ do
      parse
        "this is \\na test \\nfor newlines"
        "\"\"\"\nthis is \na test \nfor newlines\"\"\""

    it "crlfs work" $ do
      parse
        "this is\\na test"
        "\"\"\"\r\n   this is\r\n   a test\r\n\"\"\""

    it "mixing quotes work" $ do
      parse
        "string with \" in it"
        "\"\"\"\nstring with \" in it\"\"\""

    it "single quotes don't eat spaces" $ do
      parse
        "quote followed by spaces: \\'    "
        "\"\"\"\n  quote followed by spaces: \'    \"\"\""

    it "escapes don't eat spaces" $ do
      parse
        "quote followed by spaces: \\'    "
        "\"\"\"\n  quote followed by spaces: \\'    \"\"\""

    it "unicode escapes don't eat spaces" $ do
      parse
        "quote followed by spaces: \\u0020    "
        "\"\"\"\n  quote followed by spaces: \\u{0020}    \"\"\""

    it "first newline, and leading whitespace, is dropped" $ do
      parse
        "this is\\na test"
        "\"\"\"\n   this is\n   a test\n\"\"\""

    it "First proper line decides how many spaces to drop" $ do
      parse
        "this is\\n a test"
        "\"\"\"\n   this is\n    a test\n\"\"\""

    it "First proper line decides how many spaces to drop for crlf" $ do
      parse
        "this is\\n a test"
        "\"\"\"\r\n   this is\r\n    a test\r\n\"\"\""

    it "Works with differing lines" $ do
      parse
        "this is\\n a test"
        "\"\"\"\n   this is\r\n    a test\n\"\"\""

    it "Only leading spaces are dropped" $ do
      parse
        "this is\\na test"
        "\"\"\"\n   this is\n a test\n\"\"\""

    it "does not allow non-newline characters on the first line" $ do
      let isCorrectError ((Error.Syntax.String Error.Syntax.StringMultilineWithoutLeadingNewline _ _)) = True
          isCorrectError _ = False
      Helpers.checkParseError Expression.expression ExpressionBadEnd isCorrectError "\"\"\"this is not allowed\"\"\""

    it "does not allow CR without LF on the first line" $ do
      let isCorrectError ((Error.Syntax.String Error.Syntax.StringInvalidNewline _ _)) = True
          isCorrectError _ = False
      Helpers.checkParseError Expression.expression ExpressionBadEnd isCorrectError "\"\"\"\rthis is not allowed\"\"\""

    it "does not allow CR without LF on the other lines" $ do
      let isCorrectError ((Error.Syntax.String Error.Syntax.StringInvalidNewline _ _)) = True
          isCorrectError _ = False
      Helpers.checkParseError Expression.expression ExpressionBadEnd isCorrectError "\"\"\"\nthis\ris not allowed\"\"\""

parse :: String -> BS.ByteString -> IO ()
parse expectedStr =
  let isExpectedString :: Src.Pattern_ -> Bool
      isExpectedString pattern =
        case pattern of
          Src.PStr str ->
            expectedStr == Utf8.toChars str
          _ ->
            False
   in Helpers.checkSuccessfulParse (fmap (\((pat, _), loc) -> (pat, loc)) Pattern.expression) Error.Syntax.PStart isExpectedString
