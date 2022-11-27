{-# LANGUAGE OverloadedStrings #-}

module Parse.AliasSpec where

import AST.Source qualified as Src
import Data.ByteString qualified as BS
import Data.Name qualified as Name
import Helpers.Instances ()
import Parse.Module qualified as Module
import Parse.Primitives qualified as P
import Reporting.Error.Syntax qualified as Error.Syntax
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = do
  describe "Import alias" $ do
    it "regression test" $
      parse
        "Module"
        "import Some.Long.Module as Module\n"

    it "Aliases can have dots in them" $ do
      parse
        "My.Module"
        "import Some.Long.Module as My.Module\n"

parse :: String -> BS.ByteString -> IO ()
parse expectedAlias str =
  let checkResult result =
        case result of
          Right (imp, _) ->
            case Src._alias imp of
              Just (alias, _) ->
                Name.toChars alias == expectedAlias
              Nothing ->
                False
          Left _ ->
            False
   in shouldSatisfy
        (P.fromByteString Module.chompImport Error.Syntax.ModuleBadEnd str)
        checkResult
