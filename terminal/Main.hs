{-# LANGUAGE MultiWayIf #-}

module Main
  ( main,
  )
where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment qualified as Env
import Data.ByteString.Char8 qualified as BS
import Json.Decode qualified as Json
import Json.String qualified as JS
import Data.Utf8 qualified as Utf8

-- MAIN

main :: IO ()
main =
  do
    setLocaleEncoding utf8
    argStrings <- Env.getArgs
    case argStrings of
      [ json ] ->
        let jsonByteString = BS.pack json
        in case Json.fromByteString commandDecoder jsonByteString of
            Left err ->
              error (show err)

            Right command ->
              print command
      _ ->
        putStrLn "Expected exactly 1 argument: a json-encoded command"


data Command
  = Make MakeFlags
  deriving (Show)

data MakeFlags = MakeFlags
  { _make_debug :: Bool
  , _make_optimize :: Bool
  , _make_sourcemaps :: Bool
  , _make_output :: Maybe JS.String
  , _make_report_json :: Bool
  }
  deriving (Show)

data CommandDecoderError
  = UnknownCommand String
  deriving (Show)

commandDecoder :: Json.Decoder CommandDecoderError Command
commandDecoder =
  do
    tipe <- Json.field (BS.pack "command") Json.string
    if
      | tipe == (Utf8.fromChars "make") -> Make <$> makeDecoder
      | otherwise -> Json.failure (UnknownCommand $ Utf8.toChars tipe)

makeDecoder :: Json.Decoder CommandDecoderError MakeFlags
makeDecoder=
  MakeFlags
    <$> Json.field (BS.pack "debug") Json.bool
    <*> Json.field (BS.pack "optimize") Json.bool
    <*> Json.field (BS.pack "sourcemaps") Json.bool
    <*> Json.field (BS.pack "output") (Json.oneOf [ fmap Just Json.string, Json.succeed Nothing ])
    <*> Json.field (BS.pack "report-json") Json.bool
