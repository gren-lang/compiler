module Main
  ( main,
  )
where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment qualified as Env

-- MAIN

main :: IO ()
main =
  do
    setLocaleEncoding utf8
    argStrings <- Env.getArgs
    case argStrings of
      [ json ] ->
        putStrLn json

      _ ->
        putStrLn "Expected exactly 1 argument: a json-encoded command"
