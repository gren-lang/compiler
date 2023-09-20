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
  describe "Top Level Variables" $ do
    it "regression test" $
      parse "test = \"test\""

    it "Variables can be non-ascii characters" $ do
      parse "æøå = \"test\""

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
