{-# LANGUAGE OverloadedStrings #-}

module Parse.SpaceSpec where

import qualified Data.ByteString as BS
import Helpers.Instances ()
import qualified Parse.Primitives as P
import qualified Parse.Space as Space
import Test.Hspec

data ParseError x
  = SubjectError x P.Row P.Col
  | OtherError String P.Row P.Col
  deriving (Eq, Show)

spec :: Spec
spec = do
  describe "chomp" $ do
    let parseChomp = parse (Space.chomp SubjectError)
    let parseChomp3 p1 p2 = parse $ do
          () <- p1
          result <- Space.chomp SubjectError
          () <- p2
          return result

    it "parses spaces and newlines" $
      parseChomp "  \n  " `shouldBe` (Right [])

    it "parses tokens before and after" $
      parseChomp3 a b "a b" `shouldBe` (Right [])

    it "allows zero whitespace" $
      parseChomp3 a b "ab" `shouldBe` (Right [])

parse :: P.Parser (ParseError x) a -> BS.ByteString -> Either (ParseError x) a
parse parser =
  P.fromByteString parser (OtherError "fromBytString failed")

a :: P.Parser (ParseError x) ()
a = P.word1 0x61 {- a -} (OtherError "Expected 'a'")

b :: P.Parser (ParseError x) ()
b = P.word1 0x62 {- b -} (OtherError "Expected 'b'")
