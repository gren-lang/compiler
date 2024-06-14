{-# LANGUAGE OverloadedStrings #-}

module Reporting.Exit.Help
  ( Report,
    report,
    docReport,
    jsonReport,
    compilerReport,
    reportToDoc,
    reportToJson,
    syntaxErrorToDoc,
    toString,
    toStdout,
    toStderr,
  )
where

import GHC.IO.Handle (hIsTerminalDevice)
import Json.Encode ((==>))
import Json.Encode qualified as E
import Data.Maybe qualified as Maybe
import Reporting.Doc ((<+>))
import Reporting.Doc qualified as D
import Reporting.Error qualified as Error
import Reporting.Error.Syntax qualified as Error.Syntax
import Reporting.Render.Code qualified as Code
import Reporting.Report qualified as Report
import System.Environment qualified
import System.IO (Handle, hPutStr, stderr, stdout)

-- REPORT

data Report
  = CompilerReport FilePath Error.Module [Error.Module]
  | Report
      { _title :: String,
        _path :: Maybe FilePath,
        _message :: D.Doc
      }

report :: String -> Maybe FilePath -> String -> [D.Doc] -> Report
report title path startString others =
  Report title path $ D.stack (D.reflow startString : others)

docReport :: String -> Maybe FilePath -> D.Doc -> [D.Doc] -> Report
docReport title path startDoc others =
  Report title path $ D.stack (startDoc : others)

jsonReport :: String -> Maybe FilePath -> D.Doc -> Report
jsonReport =
  Report

compilerReport :: FilePath -> Error.Module -> [Error.Module] -> Report
compilerReport =
  CompilerReport

syntaxErrorToDoc :: Code.Source -> Maybe FilePath -> Error.Syntax.Error -> D.Doc
syntaxErrorToDoc source path moduleError =
  let syntaxReport = Error.Syntax.toReport source moduleError
   in reportToDoc $ Report (Report._title syntaxReport) path (Report._message syntaxReport)

-- TO DOC

reportToDoc :: Report -> D.Doc
reportToDoc report_ =
  case report_ of
    CompilerReport root e es ->
      Error.toDoc root e es
    Report title maybePath message ->
      let makeDashes n =
            replicate (max 1 (80 - n)) '-'

          errorBarEnd =
            case maybePath of
              Nothing ->
                makeDashes (4 + length title)
              Just path ->
                makeDashes (5 + length title + length path) ++ " " ++ path

          errorBar =
            D.dullcyan $
              "--" <+> D.fromChars title <+> D.fromChars errorBarEnd
       in D.stack [errorBar, message, ""]

-- TO JSON

reportToJson :: Report -> E.Value
reportToJson report_ =
  case report_ of
    CompilerReport _ e es ->
      E.object
        [ "type" ==> E.chars "compile-errors",
          "errors" ==> E.list Error.toJson (e : es)
        ]
    Report title maybePath message ->
      E.object
        [ "type" ==> E.chars "error",
          "path" ==> maybe E.null E.chars maybePath,
          "title" ==> E.chars title,
          "message" ==> D.encode message
        ]

-- OUTPUT

toString :: D.Doc -> String
toString =
  D.toString

toStdout :: D.Doc -> IO ()
toStdout doc =
  toHandle stdout doc

toStderr :: D.Doc -> IO ()
toStderr doc =
  toHandle stderr doc

toHandle :: Handle -> D.Doc -> IO ()
toHandle handle doc =
  do
    isTerminal <- hIsTerminalDevice handle
    forceColorEnv <- System.Environment.lookupEnv "FORCE_COLOR"
    let forceColor = Maybe.isJust forceColorEnv
    if isTerminal || forceColor
      then D.toAnsi handle doc
      else hPutStr handle (toString doc)
