{-# LANGUAGE OverloadedStrings #-}

module Parse.ParameterizedModulesSpec where

import AST.Source qualified as Src
import Data.ByteString qualified as BS
import Data.Name qualified as Name
import Helpers.Instances ()
import Parse.Module qualified as Module
import Parse.Primitives qualified as P
import Reporting.Annotation qualified as A
import Reporting.Error.Syntax qualified as Error.Syntax
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = do
  describe "Parameterized modules" $ do
    describe "Importing a parameterized module" $ do
      it "Imports can take arguments" $
        parseImport
          ["ModA"]
          "import ParamModule(ModA)"
      it "Imports can take two arguments" $
        parseImport
          ["ModA", "ModB"]
          "import ParamModule(ModA, ModB)"
      it "Imports can take three arguments (and more)" $
        parseImport
          ["ModA", "ModB", "ModC"]
          "import ParamModule(ModA, ModB, ModC)"
      it "Imports can take a complex argument" $
        parseImport
          ["Some.Module"]
          "import ParamModule(Some.Module)"

    describe "Defining parameterized module" $ do
      it "Modules can take a parameter" $
        parseModule
          [("One", "SomeSignature")]
          "module ParamModule(One : SomeSignature) exposing (..)"

parseImport :: [String] -> BS.ByteString -> IO ()
parseImport expectedArgs str =
  let checkResult result =
        case result of
          Right (import_, _) ->
            let args = map localizedNameToString (Src._args import_)
             in expectedArgs == args
          Left _ ->
            False

      strWithNewline = str <> "\n"
   in shouldSatisfy
        (P.fromByteString Module.chompImport Error.Syntax.ModuleBadEnd strWithNewline)
        checkResult

parseModule :: [(String, String)] -> BS.ByteString -> IO ()
parseModule expectedParams str =
  let checkResult result =
        case result of
          Right module_ ->
            let params = map (\(alias, type_) -> (localizedNameToString alias, localizedNameToString type_)) (Src._params module_)
             in expectedParams == params
          Left _ ->
            False
      validModuleStr = str <> "\n\none = 1"
   in shouldSatisfy
        (Module.fromByteString Module.Application validModuleStr)
        checkResult

localizedNameToString :: A.Located Name.Name -> String
localizedNameToString localizedName =
  Name.toChars $ A.toValue localizedName
