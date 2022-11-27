{-# LANGUAGE OverloadedStrings #-}

module Parse.SpaceSpec where

import AST.Source (Comment, Comment_ (..))
import Data.ByteString qualified as BS
import Data.Word (Word16)
import Helpers.Instances ()
import Parse.Primitives qualified as P
import Parse.Space qualified as Space
import Reporting.Annotation qualified as A
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
      parseChomp "  \n  " `shouldParseComments` Right []

    it "parses tokens before and after" $
      parseChomp3 a b "a b" `shouldParseComments` Right []

    it "allows zero whitespace" $
      parseChomp3 a b "ab" `shouldParseComments` Right []

    it "parses curly brace comments" $
      parseChomp "{- 1 -}"
        `shouldParseComments` Right [at 1 1 1 8 $ BlockComment " 1 "]

    it "can parse curly brace token adjacent to whitespace" $
      parseChomp3 leftCurly leftCurly "{{- 1 -} {"
        `shouldParseComments` Right [at 1 2 1 9 $ BlockComment " 1 "]

    it "can parse nested curly brace comments" $
      parseChomp "{- {- inner -} outer -}"
        `shouldParseComments` Right [at 1 1 1 24 $ BlockComment " {- inner -} outer "]

    it "parses hyphen comments" $
      parseChomp "-- 1\n" `shouldParseComments` Right [at 1 1 1 5 $ LineComment " 1"]

    it "parses hyphen comments at end of file" $
      parseChomp "-- 1" `shouldParseComments` Right [at 1 1 1 5 $ LineComment " 1"]

    it "can parse hyphen adjacent to whitespace" $
      parseChomp3 hyphen hyphen "- -- 1\n-" `shouldParseComments` Right [at 1 3 1 7 $ LineComment " 1"]

    it "can parse nested hyphen comments" $
      parseChomp "-- outer -- inner" `shouldParseComments` Right [at 1 1 1 18 $ LineComment " outer -- inner"]

    it "returns comments in the correct order" $
      parseChomp "{- 1 -}{- 2 -}  -- 3\n{- 4 -}\n{- 5 -}"
        `shouldParseComments` Right
          [ at 1 1 1 8 $ BlockComment " 1 ",
            at 1 8 1 15 $ BlockComment " 2 ",
            at 1 17 1 21 $ LineComment " 3",
            at 2 1 2 8 $ BlockComment " 4 ",
            at 3 1 3 8 $ BlockComment " 5 "
          ]

parse :: P.Parser (ParseError x) a -> BS.ByteString -> Either (ParseError x) a
parse parser =
  P.fromByteString parser (OtherError "fromByteString failed")

shouldParseComments :: (Eq x, Show x) => Either x [Comment] -> Either x [Comment] -> IO ()
shouldParseComments actual expected =
  fmap (fmap locatedToTuple) actual
    `shouldBe` fmap (fmap locatedToTuple) expected
  where
    locatedToTuple (A.At (A.Region start end) comment) =
      ((start, end), comment)

a :: P.Parser (ParseError x) ()
a = P.word1 0x61 {- a -} (OtherError "Expected 'a'")

b :: P.Parser (ParseError x) ()
b = P.word1 0x62 {- b -} (OtherError "Expected 'b'")

leftCurly :: P.Parser (ParseError x) ()
leftCurly = P.word1 0x7B {- { -} (OtherError "Expected '{'")

hyphen :: P.Parser (ParseError x) ()
hyphen = P.word1 0x2D {- - -} (OtherError "Expected '-'")

at :: Word16 -> Word16 -> Word16 -> Word16 -> a -> A.Located a
at startRow startCol endRow endCol =
  A.At (A.Region (A.Position startRow startCol) (A.Position endRow endCol))
