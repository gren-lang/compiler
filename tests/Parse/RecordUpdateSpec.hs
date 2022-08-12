{-# LANGUAGE OverloadedStrings #-}

module Parse.RecordUpdateSpec where

import AST.Source qualified as Src
import Data.ByteString qualified as BS
import Helpers.Instances ()
import Parse.Expression (expression)
import Parse.Primitives qualified as P
import Reporting.Annotation qualified as A
import Test.Hspec

data ParseError
  = ExprError P.Row P.Col
  | OtherError String P.Row P.Col
  deriving (Show, Eq)

spec :: Spec
spec = do
  describe "record update" $ do
    it "regression test" $
      parseRecordLiteral "{ field = 2 }"

    it "regression test with multiple fields" $
      parseRecordLiteral "{ f1 = 1, f2 = 2, f3 = 3 }"

    it "basic case" $
      parse "{ record | prop = 1 }"

    it "qualified var" $
      parse "{ Module.record | prop = 1 }"

    it "nested var" $
      parse "{ Module.record.nested | prop = 1 }"

    it "update literal record" $
      parse "{ { prop = 2 } | prop = 1 }"

    it "parenthesized if statement" $
      parse "{ (if 1 == 2 then { prop = 2 } else { prop = 3 }) | prop = 1 }"

    it "parenthesized if statement with || operator" $
      parse "{ (if left || right then { prop = 2 } else { prop = 3 }) | prop = 1 }"

--

parse :: BS.ByteString -> IO ()
parse str =
  ( P.fromByteString
      (P.specialize (\_ row col -> ExprError row col) expression)
      (OtherError "fromByteString failed")
      str
  )
    `shouldSatisfy` isUpdateExpr

isUpdateExpr :: Either x (Src.Expr, A.Position) -> Bool
isUpdateExpr result =
  case result of
    Right (A.At _ (Src.Update _ _), _) -> True
    _ -> False

--

parseRecordLiteral :: BS.ByteString -> IO ()
parseRecordLiteral str =
  ( P.fromByteString
      (P.specialize (\_ row col -> ExprError row col) expression)
      (OtherError "fromByteString failed")
      str
  )
    `shouldSatisfy` isRecordLiteral

isRecordLiteral :: Either x (Src.Expr, A.Position) -> Bool
isRecordLiteral result =
  case result of
    Right (A.At _ (Src.Record _), _) -> True
    _ -> False
