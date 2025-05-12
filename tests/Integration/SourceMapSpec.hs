{-# LANGUAGE OverloadedStrings #-}

module Integration.SourceMapSpec (spec) where

import Control.Exception (bracket)
import System.IO.Temp (withSystemTempDirectory)

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy as BL
import Data.Vector qualified as V

import Gren.Platform qualified as Platform
import Init qualified
import Make qualified
import System.Directory (copyFile, getCurrentDirectory, setCurrentDirectory)
import Test.Hspec

import Init (Flags (_skipPrompts), _isPackage, _platform)
import Make (Flags (_optimize, _output, _report, _sourceMaps))
import System.FilePath ((</>))

{-
Run the tests for this module (only), with:

Option 1 (cabal + rg):
  cabal test gren-tests --test-options="-m Integration.SourceMapSpec"
  cabal run test:gren-tests -- -m Integration.SourceMapSpec
  rg --files | entr -c cabal run test:gren-tests -- -m Integration.SourceMapSpec

Option 2 (ghcid):
  ghcid -c 'cabal repl test:gren-tests' -T ":main -m Integration.SourceMapSpec" --warning

-}

spec :: Spec
spec =
  describe "Integration.SourceMapSpec" $ do
    it "compiles a project with source maps and a unique top-level source-directories option" $ do
      bracket
        getCurrentDirectory
        setCurrentDirectory
        ( \projectDir -> do
            withSystemTempDirectory "gren-test-" $ \tempDir -> do
              setCurrentDirectory tempDir
              Init.run $
                Init.Flags
                  { _skipPrompts = True
                  , _isPackage = False
                  , _platform = Platform.Node
                  }

              copyFile
                (projectDir </> "./tests/Integration/Fixtures/Main.gren")
                (tempDir </> "./Main.gren")

              content <- BL.readFile (tempDir </> "./gren.json")
              case decode content :: Maybe Object of
                Nothing ->
                  fail "failed to decode gren.json"
                Just obj -> do
                  let newObj =
                        KM.insert
                          "source-directories"
                          (Array $ V.fromList [String "."])
                          obj
                  BL.writeFile
                    (tempDir </> "./gren.json")
                    (encode newObj)

                  Make.run
                    ["./Main.gren"]
                    ( Make.Flags
                        { _optimize = False
                        , _sourceMaps = True
                        , _output = Nothing
                        , _report = False
                        }
                    )
        )
