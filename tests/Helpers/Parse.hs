{-# LANGUAGE OverloadedStrings #-}

module Helpers.Parse
  ( checkParse,
    checkSuccessfulParse,
    checkParseError,
  )
where

import Data.ByteString qualified as BS
import Parse.Primitives qualified as P
import Parse.Space qualified as Space
import Reporting.Annotation qualified as A
import Test.Hspec qualified as Hspec

checkParse :: (Show error, Show target) => Space.Parser error (A.Located target) -> (P.Row -> P.Col -> error) -> (Either error (A.Located target, A.Position) -> Bool) -> BS.ByteString -> IO ()
checkParse parser toBadEnd checkResult str =
  Hspec.shouldSatisfy
    (P.fromByteString parser toBadEnd str)
    checkResult

checkSuccessfulParse :: (Show error, Show target) => Space.Parser error (A.Located target) -> (P.Row -> P.Col -> error) -> (target -> Bool) -> BS.ByteString -> IO ()
checkSuccessfulParse parser toBadEnd checkTarget =
  let checkResult result =
        case result of
          Right (A.At _ target, _) ->
            checkTarget target
          Left _ ->
            False
   in checkParse parser toBadEnd checkResult

checkParseError :: (Show error, Show target) => Space.Parser error (A.Located target) -> (P.Row -> P.Col -> error) -> (error -> Bool) -> BS.ByteString -> IO ()
checkParseError parser toBadEnd checkError =
  let checkResult result =
        case result of
          Left err ->
            checkError err
          Right _ ->
            False
   in checkParse parser toBadEnd checkResult
