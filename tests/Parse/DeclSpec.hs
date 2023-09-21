{-# LANGUAGE OverloadedStrings #-}

module Parse.DeclSpec where

import Data.ByteString qualified as BS
import Helpers.Instances ()
import Parse.Declaration (declaration)
import Parse.Primitives qualified as P
import Test.Hspec (Spec, describe, it, shouldSatisfy)

data ParseError
  = DeclError P.Row P.Col
  | OtherError String P.Row P.Col
  deriving (Show, Eq)

spec :: Spec
spec = do
  describe "Top Level Valeus" $ do
    it "regression test" $
      parse "test = 1"

    it "Value names can contain non-ascii characters" $ do
      parse "vålue = 1"

    it "Value names can be only non-ascii characters" $ do
      parse "æøå = 1"

parse :: BS.ByteString -> IO ()
parse str =
  P.fromByteString
    (P.specialize (\_ row col -> DeclError row col) declaration)
    (OtherError "fromByteString failed")
    str
    `shouldSatisfy` valid

valid :: Either x y -> Bool
valid result =
  case result of
    Right _ -> True
    Left _ -> False
