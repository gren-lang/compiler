{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Generate.Node
  ( sandwich,
    leadingLines,
  )
where

import Data.ByteString.Builder qualified as B
import Data.Name qualified as Name
import Text.RawString.QQ (r)

leadingLines :: Int
leadingLines = 3

sandwich :: Name.Name -> B.Builder -> B.Builder
sandwich moduleName javascript =
  let name = Name.toBuilder moduleName
   in [r|#!/usr/bin/env node

try {
|]
        <> javascript
        <> [r|
|]
        <> [r|this.Gren.|]
        <> name
        <> [r|.init({});
}
catch (e)
{
console.error(e);
}
|]
