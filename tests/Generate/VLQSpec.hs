module Generate.VLQSpec (spec) where

import Generate.VLQ (encode)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "VLQ tests" $ do
    it "Encodes from Int to String" $ do
      encode 0 `shouldBe` "A"
      encode 1 `shouldBe` "C"
      encode (-1) `shouldBe` "D"
      encode 3 `shouldBe` "G"
      encode 123 `shouldBe` "2H"
      encode 123456789 `shouldBe` "qxmvrH"
      -- limits:
      encode (-2147483648) `shouldBe` "B"
      encode 2147483647 `shouldBe` "+/////D"
