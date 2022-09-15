module AST.SourceComments where

import Data.Utf8 qualified as Utf8

data GREN_COMMENT

data Comment
  = BlockComment (Utf8.Utf8 GREN_COMMENT)
  | LineComment (Utf8.Utf8 GREN_COMMENT)
  deriving (Show)

-- Manager

data CmdComments = CmdComments
  { _beforeCommandKeyword :: [Comment],
    _afterCommand :: [Comment]
  }
  deriving (Show)

data SubComments = SubComments
  { _beforeSubscriptionsKeyword :: [Comment],
    _afterSubscriptions :: [Comment]
  }
  deriving (Show)

data FxComments = FxComments
  { _cmdComments :: CmdComments,
    _subComments :: SubComments
  }
  deriving (Show)
