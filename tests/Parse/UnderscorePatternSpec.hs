{-# LANGUAGE OverloadedStrings #-}

module Parse.UnderscorePatternSpec where

import AST.Source qualified as Src
import Data.ByteString qualified as BS
import Helpers.Instances ()
import Parse.Pattern qualified as Pattern
import Parse.Primitives qualified as P
import Reporting.Annotation qualified as A
import Test.Hspec ( Spec, describe, it) 
import Test.Hspec qualified as Hspec 

data ParseError
  = ExprError P.Row P.Col
  | OtherError String P.Row P.Col
  deriving (Show, Eq)

spec :: Spec
spec = do
  describe "Wildcard patterns" $ do
    it "regression test" $
      parse "_"
    it "Newly allowed named wildcard pattern" $ do
      parse "_argument"
    it "You can have underscores as part of the lower variable which follows the underscore" $ do
      parse "_hello_world"
    it "Keywords are not allowed as the whole variable part of an underscore pattern" $ do
      failToParse "_let"
    it "But you can have a keyword as **part** of a variable name just as for normal variable names." $ do
      parse "_let_down"
    it "But you cannot start with multiple underscores" $ do
      failToParse "__hello"
    it "But it must be an lower name, for an underscore pattern" $ do
      failToParse "_Hello"

attemptParse :: (Either ParseError (Src.Pattern, A.Position) -> Bool) -> BS.ByteString -> IO ()
attemptParse checkResult str =
  Hspec.shouldSatisfy
      ( P.fromByteString
          (P.specialize (\_ row col -> ExprError row col) Pattern.expression)
          (OtherError "fromByteString failed")
          str
      )
      checkResult

parse :: BS.ByteString -> IO ()
parse =
  let
    isWildCardPattern :: Either x (Src.Pattern, A.Position) -> Bool
    isWildCardPattern result =
      case result of
        Right (A.At _ Src.PAnything, _) -> True
        _ -> False
  in
  attemptParse isWildCardPattern


failToParse :: BS.ByteString -> IO ()
failToParse =
  let
     isError :: Either x (Src.Pattern, A.Position) -> Bool
     isError result =
       case result of
          Left _ ->
            True
          _ ->
            False
  in
  attemptParse isError
