{-# LANGUAGE OverloadedStrings #-}

module Parse.SpaceSpec where

import AST.Source (Comment (..))
import Data.ByteString qualified as BS
import Helpers.Instances ()
import Parse.Primitives qualified as P
import Parse.Space qualified as Space
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
      parseChomp "  \n  " `shouldBe` Right []

    it "parses tokens before and after" $
      parseChomp3 a b "a b" `shouldBe` Right []

    it "allows zero whitespace" $
      parseChomp3 a b "ab" `shouldBe` Right []

    it "parses curly brace comments" $
      parseChomp "{- 1 -}" `shouldBe` Right [BlockComment " 1 "]

    it "can parse curly brace token adjacent to whitespace" $
      parseChomp3 leftCurly leftCurly "{{- 1 -} {"
        `shouldBe` Right [BlockComment " 1 "]

    it "can parse nested curly brace comments" $
      parseChomp "{- {- inner -} outer -}"
        `shouldBe` Right [BlockComment " {- inner -} outer "]

    it "parses hyphen comments" $
      parseChomp "-- 1\n" `shouldBe` Right [LineComment " 1"]

    it "parses hyphen comments at end of file" $
      parseChomp "-- 1" `shouldBe` Right [LineComment " 1"]

    it "can parse hyphen adjacent to whitespace" $
      parseChomp3 hyphen hyphen "- -- 1\n-" `shouldBe` Right [LineComment " 1"]

    it "can parse nested hyphen comments" $
      parseChomp "-- outer -- inner" `shouldBe` Right [LineComment " outer -- inner"]

    it "returns comments in the correct order" $
      parseChomp "{- 1 -}{- 2 -}  -- 3\n{- 4 -}\n{- 5 -}"
        `shouldBe` Right
          [ BlockComment " 1 ",
            BlockComment " 2 ",
            LineComment " 3",
            BlockComment " 4 ",
            BlockComment " 5 "
          ]

parse :: P.Parser (ParseError x) a -> BS.ByteString -> Either (ParseError x) a
parse parser =
  P.fromByteString parser (OtherError "fromByteString failed")

a :: P.Parser (ParseError x) ()
a = P.word1 0x61 {- a -} (OtherError "Expected 'a'")

b :: P.Parser (ParseError x) ()
b = P.word1 0x62 {- b -} (OtherError "Expected 'b'")

leftCurly :: P.Parser (ParseError x) ()
leftCurly = P.word1 0x7B {- { -} (OtherError "Expected '{'")

hyphen :: P.Parser (ParseError x) ()
hyphen = P.word1 0x2D {- - -} (OtherError "Expected '-'")
