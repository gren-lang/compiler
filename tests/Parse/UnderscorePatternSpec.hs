{-# LANGUAGE OverloadedStrings #-}

module Parse.UnderscorePatternSpec where

import AST.Source qualified as Src
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as B
import Data.Name qualified as Name
import Helpers.Instances ()
import Parse.Expression qualified as Expression
import Parse.Helpers qualified as Helpers
import Parse.Pattern qualified as Pattern
import Parse.Primitives qualified as P
import Reporting.Annotation qualified as A
import Reporting.Error.Syntax qualified as Error.Syntax
import Test.Hspec (Spec, describe, it)
import Test.Hspec qualified as Hspec

spec :: Spec
spec = do
  describe "Wildcard patterns" $ do
    it "regression test" $
      parse "" "_"
    it "Newly allowed named wildcard pattern" $
      parse "argument" "_argument"
    it "You can have underscores as part of the lower variable which follows the underscore" $
      parse "hello_world" "_hello_world"
    it "Keywords are not allowed as the whole variable part of an underscore pattern" $
      failToParse "_let"
    it "But you can have a keyword as **part** of a variable name just as for normal variable names." $
      parse "let_down" "_let_down"
    it "But you cannot start with multiple underscores" $
      failToParse "__hello"
    it "But it must be a lower name, for an underscore pattern" $
      failToParse "_Hello"
    it "We should give the specialised error when we attempt to parse _key as an expression" $
      let isWildCardAttemptError :: Error.Syntax.Expr -> Bool
          isWildCardAttemptError error =
            case error of
              Error.Syntax.WildCard (Error.Syntax.WildCardAttempt _) _ _ ->
                True
              _ ->
                False
       in Helpers.checkParseError Expression.expression Error.Syntax.Start isWildCardAttemptError "_key"

parse :: String -> BS.ByteString -> IO ()
parse expectedName =
  let isWildCardPattern :: Src.Pattern_ -> Bool
      isWildCardPattern pattern =
        case pattern of
          Src.PAnything name ->
            expectedName == (Name.toChars name)
          _ ->
            False
   in Helpers.checkSuccessfulParse Pattern.expression Error.Syntax.PStart isWildCardPattern

failToParse :: BS.ByteString -> IO ()
failToParse =
  Helpers.checkParseError Pattern.expression Error.Syntax.PStart (\_ -> True)
