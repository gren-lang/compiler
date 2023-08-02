{-# LANGUAGE OverloadedStrings #-}

module Parse.ParameterizedModulesSpec where

import AST.Source qualified as Src
import Data.ByteString qualified as BS
import Data.Name qualified as Name
import Helpers.Instances ()
import Parse.Module qualified as Module
import Parse.Primitives qualified as P
import Reporting.Error.Syntax qualified as Error.Syntax
import Reporting.Annotation qualified as A
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = do
  describe "Parameterized modules" $ do
    it "Imports can take a parameter" $
      parse
        ["ModA"]
        "import ParamModule(ModA)"
    
    it "Imports can take two parameters" $
      parse
        ["ModA", "ModB"]
        "import ParamModule(ModA, ModB)"
    it "Imports can take three parameters (and more)" $
      parse
        ["ModA", "ModB", "ModC"]
        "import ParamModule(ModA, ModB, ModC)"

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
