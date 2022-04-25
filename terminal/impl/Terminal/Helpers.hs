{-# LANGUAGE OverloadedStrings #-}

module Terminal.Helpers
  ( version,
    grenFile,
    grenFileOrDirectory,
    package,
  )
where

import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.Char as Char
import qualified Data.Utf8 as Utf8
import qualified Gren.Package as Pkg
import qualified Gren.Version as V
import qualified Parse.Primitives as P
import qualified System.FilePath as FP
import Terminal (Parser (..))

-- VERSION

version :: Parser V.Version
version =
  Parser
    { _singular = "version",
      _plural = "versions",
      _parser = parseVersion,
      _suggest = suggestVersion,
      _examples = return . exampleVersions
    }

parseVersion :: String -> Maybe V.Version
parseVersion chars =
  case P.fromByteString V.parser (,) (BS_UTF8.fromString chars) of
    Right vsn -> Just vsn
    Left _ -> Nothing

suggestVersion :: String -> IO [String]
suggestVersion _ =
  return []

exampleVersions :: String -> [String]
exampleVersions chars =
  let chunks = map Utf8.toChars (Utf8.split 0x2E {-.-} (Utf8.fromChars chars))
      isNumber cs = not (null cs) && all Char.isDigit cs
   in if all isNumber chunks
        then case chunks of
          [x] -> [x ++ ".0.0"]
          [x, y] -> [x ++ "." ++ y ++ ".0"]
          x : y : z : _ -> [x ++ "." ++ y ++ "." ++ z]
          _ -> ["1.0.0", "2.0.3"]
        else ["1.0.0", "2.0.3"]

-- GREN FILE

grenFile :: Parser FilePath
grenFile =
  Parser
    { _singular = "gren file",
      _plural = "gren files",
      _parser = parseGrenFile,
      _suggest = \_ -> return [],
      _examples = exampleGrenFiles
    }

parseGrenFile :: String -> Maybe FilePath
parseGrenFile chars =
  if FP.takeExtension chars == ".gren"
    then Just chars
    else Nothing

exampleGrenFiles :: String -> IO [String]
exampleGrenFiles _ =
  return ["Main.gren", "src/Main.gren"]

-- GREN FILE OR DIRECTORY

grenFileOrDirectory :: Parser FilePath
grenFileOrDirectory =
  Parser
    { _singular = "gren file or directory",
      _plural = "gren files and/or directories",
      _parser = Just,
      _suggest = \_ -> return [],
      _examples = \_ -> return ["Main.gren", "src/Examples/"]
    }

-- PACKAGE

package :: Parser Pkg.Name
package =
  Parser
    { _singular = "package",
      _plural = "packages",
      _parser = parsePackage,
      _suggest = (\_ -> return []),
      _examples = \_ -> return []
    }

parsePackage :: String -> Maybe Pkg.Name
parsePackage chars =
  case P.fromByteString Pkg.parser (,) (BS_UTF8.fromString chars) of
    Right pkg -> Just pkg
    Left _ -> Nothing
