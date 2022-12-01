module AST.SourceComments where

import Data.Utf8 qualified as Utf8
import Reporting.Annotation qualified as A

data GREN_COMMENT

type Comment = A.Located Comment_

data Comment_
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
  deriving (Show)

data ImportExposingComments = ImportExposingComments
  { _afterExposing :: [Comment],
    _afterExposingListing :: [Comment]
  }
  deriving (Show)

-- Declarations

data ValueComments = ValueComments
  { _beforeValueEquals :: [Comment],
    _beforeValueBody :: [Comment],
    _afterValueBody :: [Comment]
  }
  deriving (Show)

data ValueTypeComments = ValueTypeComments
  { _beforeTypeColon :: [Comment],
    _afterTypeColon :: [Comment],
    _afterValueType :: [Comment]
  }
  deriving (Show)

-- Expressions

data BinopsSegmentComments = BinopsSegmentComments
  { _beforeOperator :: [Comment],
    _afterOperator :: [Comment]
  }
  deriving (Show)

data ArrayEntryComments = ArrayEntryComments
  { _beforeArrayEntry :: [Comment],
    _afterArrayEntry :: [Comment]
  }
  deriving (Show)

data LambdaComments = LambdaComments
  { _beforeArrow :: [Comment],
    _afterArrow :: [Comment]
  }
  deriving (Show)

data IfComments = IfComments
  { _beforeElseBody :: [Comment],
    _afterElseBody :: [Comment]
  }
  deriving (Show)

data IfBranchComments = IfBranchComments
  { _afterIfKeyword :: [Comment],
    _beforeThenKeyword :: [Comment],
    _beforeThenBody :: [Comment],
    _afterThenBody :: [Comment]
  }
  deriving (Show)

data LetComments = LetComments
  { _afterLetDecls :: [Comment],
    _afterIn :: [Comment]
  }
  deriving (Show)

data CaseComments = CaseComments
  { _afterCaseKeyword :: [Comment],
    _beforeOfKeyword :: [Comment]
  }
  deriving (Show)

data CaseBranchComments = CaseBranchComments
  { _beforeBranch :: [Comment],
    _beforeBranchArrow :: [Comment],
    _beforeBranchBody :: [Comment],
    _afterBranchBody :: [Comment]
  }
  deriving (Show)

data UpdateComments = UpdateComments
  { _beforeBase :: [Comment],
    _afterBase :: [Comment]
  }
  deriving (Show)

data RecordFieldComments = RecordFieldComments
  { _beforeFieldName :: [Comment],
    _afterFieldName :: [Comment],
    _beforeFieldValue :: [Comment],
    _afterFieldValue :: [Comment]
  }
  deriving (Show)

-- Patterns

data PArrayEntryComments = PArrayEntryComments
  { _beforePArrayEntry :: [Comment],
    _afterPArrayEntry :: [Comment]
  }
  deriving (Show)

-- Types

data TParensComments = TParensComments
  { _afterOpening :: [Comment],
    _beforeClosing :: [Comment]
  }
  deriving (Show)
