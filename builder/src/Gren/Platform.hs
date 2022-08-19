module Gren.Platform
  ( Platform (..),
    --
    compatible,
    --
    encode,
    decoder,
  )
where

import Json.Decode qualified as D
import Json.Encode qualified as E
import Json.String qualified as Json
import Reporting.Exit qualified as Exit

data Platform
  = Common
  | Browser
  | Node
  deriving (Eq)

-- COMPATIBILITY

compatible :: Platform -> Platform -> Bool
compatible rootPlatform comparison =
  rootPlatform == comparison || comparison == Common

-- JSON

encode :: Platform -> E.Value
encode platform =
  case platform of
    Common -> E.chars "common"
    Browser -> E.chars "browser"
    Node -> E.chars "node"

decoder :: D.Decoder Exit.OutlineProblem Platform
decoder =
  let common = Json.fromChars "common"
      browser = Json.fromChars "browser"
      node = Json.fromChars "node"
   in do
        platform <- D.string
        if platform == common
          then D.succeed Common
          else
            if platform == browser
              then D.succeed Browser
              else
                if platform == node
                  then D.succeed Node
                  else D.failure Exit.OP_BadPlatform
