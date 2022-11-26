module AST.SourceComments where

import Data.Utf8 qualified as Utf8

data GREN_COMMENT

data Comment
  = BlockComment (Utf8.Utf8 GREN_COMMENT)
  | LineComment (Utf8.Utf8 GREN_COMMENT)
  deriving (Eq, Show)

-- Module

data HeaderComments = HeaderComments
  { _beforeModuleLine :: [Comment],
    _afterModuleKeyword :: [Comment],
    _afterModuleName :: [Comment],
    _afterExposingKeyword :: [Comment],
    _afterModuleLine :: [Comment],
    _afterModuleDocComment :: [Comment]
  }
  deriving (Show)

-- Effects

data PortsComments = PortsComments
  { _afterPortKeyword :: [Comment]
  }
  deriving (Show)

data ManagerComments = ManagerComments
  { _afterEffectKeyword :: [Comment],
    _afterWhereKeyword :: [Comment],
    _afterManager :: [Comment]
  }
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

-- Import

data ImportComments = ImportComments
  { _afterImportKeyword :: [Comment],
    _afterImportName :: [Comment]
  }
  deriving (Show)

data ImportAliasComments = ImportAliasComments
  { _afterAs :: [Comment],
    _afterAliasName :: [Comment]
  }
  deriving (Eq, Show)

data ImportExposingComments = ImportExposingComments
  { _afterExposing :: [Comment],
    _afterExposingListing :: [Comment]
  }
  deriving (Show)
