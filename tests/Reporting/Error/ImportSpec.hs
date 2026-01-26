module Reporting.Error.ImportSpec (spec) where

import Data.Set qualified as Set
import Data.Text as T
import Data.Text.Encoding as T
import Data.Utf8 qualified as Utf8
import Prettyprinter qualified as P
import Prettyprinter.Render.Text (renderStrict)
import Reporting.Annotation qualified as A
import Reporting.Error.Import qualified as Import
import Reporting.Render.Code qualified as Code
import Reporting.Report qualified as Report
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  describe "Import error reporting" $ do
    {-
        When "gren-lang/core" module is not listed in gren.json, the
        default imports that the compiler inserts into the code
        fail, but there is no source code region to report.
        We want to ensure the reporting code doesn't crash, and
        actually reports that modules like Basics (and others) are reported
        as not found.
    -}
    it "Give proper error if default package dependencies are not found" $ do
      let -- Empty source code
          source = Code.toSource (T.encodeUtf8 (T.pack ""))

          -- The "Module Not Found" Error
          err =
            Import.Error
              { -- IMPORTANT: The region here is zero, not pointing to any
                -- possible lines of source code. This happens when
                -- "import Basics" is added by default by the compiler, without
                -- it appearing in the actual source code.
                Import._region = A.Region (A.Position 0 0) (A.Position 0 0),
                Import._import = Utf8.fromChars "foo",
                Import._unimported = Set.singleton (Utf8.fromChars "Basics"),
                Import._problem = Import.NotFound
              }

          -- Create the report
          report = Import.toReport source err

          -- Convert the Prettyprinter Doc to a Data.Text
          messageText = renderStrict (P.layoutCompact $ Report._message report)
       in -- Does it have one of the missing modules reported in it?
          T.isInfixOf (T.pack "Basics") messageText
