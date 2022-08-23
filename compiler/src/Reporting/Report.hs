{-# LANGUAGE OverloadedStrings #-}

module Reporting.Report
  ( Report (..),
  )
where

import Reporting.Annotation qualified as A
import Reporting.Doc qualified as D

-- BUILD REPORTS

data Report = Report
  { _title :: String,
    _region :: A.Region,
    _sgstns :: [String],
    _message :: D.Doc
  }
