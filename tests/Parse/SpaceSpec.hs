{-# LANGUAGE OverloadedStrings #-}

module Parse.SpaceSpec where

import Helpers.Instances ()
import qualified Parse.Primitives as P
import qualified Parse.Space as Space
import Test.Hspec

spec :: Spec
spec = do
  describe "chomp" $ do
    let parseChomp = P.fromByteString (Space.chomp (\e _ _ -> Just e)) (\_ _ -> Nothing)

    it "parses spaces and newlines" $
      parseChomp "  \n  " `shouldBe` (Right [])
