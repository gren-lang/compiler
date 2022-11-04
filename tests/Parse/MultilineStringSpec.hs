{-# LANGUAGE OverloadedStrings #-}

module Parse.MultilineStringSpec where

import AST.Source qualified as Src
import Data.ByteString qualified as BS
import Data.Utf8 qualified as Utf8
import Helpers.Instances ()
import Helpers.Parse qualified as Helpers
import Parse.Pattern qualified as Pattern
import Reporting.Error.Syntax qualified as Error.Syntax
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  describe "Multiline String" $ do
    it "regression test" $
      parse "normal string" "\"normal string\""

parse :: String -> BS.ByteString -> IO ()
parse expectedStr =
  let isExpectedString :: Src.Pattern_ -> Bool
      isExpectedString pattern =
        case pattern of
          Src.PStr str ->
            expectedStr == Utf8.toChars str
          _ ->
            False
   in Helpers.checkSuccessfulParse Pattern.expression Error.Syntax.PStart isExpectedString

