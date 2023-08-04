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
    it "Imports can take arguments" $
      parse
        ["ModA"]
        "import ParamModule(ModA)\n"
    it "Imports can take two arguments" $
      parse
        ["ModA", "ModB"]
        "import ParamModule(ModA, ModB)\n"
    it "Imports can take three arguments (and more)" $
      parse
        ["ModA", "ModB", "ModC"]
        "import ParamModule(ModA, ModB, ModC)\n"
    it "Imports can take a complex argument" $
      parse
        ["Some.Module"]
        "import ParamModule(Some.Module)\n"

parse :: [String] -> BS.ByteString -> IO ()
parse expectedParams str =
  let checkResult result =
        case result of
          Right (imp, _) ->
            let params = map (Name.toChars . A.toValue) (Src._params imp)
             in expectedParams == params
          Left _ ->
            False
   in shouldSatisfy
        (P.fromByteString Module.chompImport Error.Syntax.ModuleBadEnd str)
        checkResult
