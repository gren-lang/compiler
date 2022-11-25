{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Reporting.Error.Pattern
  ( P.Error (..),
    toReport,
  )
where

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Name qualified as Name
import Gren.String qualified as ES
import Nitpick.PatternMatches qualified as P
import Reporting.Doc qualified as D
import Reporting.Render.Code qualified as Code
import Reporting.Report qualified as Report

-- TO REPORT

toReport :: Code.Source -> P.Error -> Report.Report
toReport source err =
  case err of
    P.Redundant caseRegion patternRegion index ->
      Report.Report "REDUNDANT PATTERN" patternRegion [] $
        Code.toSnippet
          source
          caseRegion
          (Just patternRegion)
          ( D.reflow $
              "The " <> D.intToOrdinal index <> " pattern is redundant:",
            D.reflow $
              "Any value with this shape will be handled by a previous\
              \ pattern, so it should be removed."
          )
    P.Incomplete region context unhandled ->
      case context of
        P.BadArg ->
          Report.Report "UNSAFE PATTERN" region [] $
            Code.toSnippet
              source
              region
              Nothing
              ( "This pattern does not cover all possibilities:",
                D.stack
                  [ "Other possibilities include:",
                    unhandledPatternsToDocBlock unhandled,
                    D.reflow $
                      "I would have to crash if I saw one of those! So rather than\
                      \ pattern matching in function arguments, put a `case` in\
                      \ the function body to account for all possibilities."
                  ]
              )
        P.BadDestruct ->
          Report.Report "UNSAFE PATTERN" region [] $
            Code.toSnippet
              source
              region
              Nothing
              ( "This pattern does not cover all possible values:",
                D.stack
                  [ "Other possibilities include:",
                    unhandledPatternsToDocBlock unhandled,
                    D.reflow $
                      "I would have to crash if I saw one of those! You can use\
                      \ `let` to deconstruct values only if there is ONE possibility.\
                      \ Switch to a `case` expression to account for all possibilities.",
                    D.toSimpleHint $
                      "Are you calling a function that definitely returns values\
                      \ with a very specific shape? Try making the return type of\
                      \ that function more specific!"
                  ]
              )
        P.BadCase ->
          Report.Report "MISSING PATTERNS" region [] $
            Code.toSnippet
              source
              region
              Nothing
              ( "This `case` does not have branches for all possibilities:",
                D.stack
                  [ "Missing possibilities include:",
                    unhandledPatternsToDocBlock unhandled,
                    D.reflow $
                      "I would have to crash if I saw one of those. Add branches for them!",
                    D.link
                      "Hint"
                      "If you want to write the code for each branch later, use `Debug.todo` as a placeholder. Read"
                      "missing-patterns"
                      "for more guidance on this workflow."
                  ]
              )

-- PATTERN TO DOC

unhandledPatternsToDocBlock :: [P.Pattern] -> D.Doc
unhandledPatternsToDocBlock unhandledPatterns =
  D.indent 4 $
    D.dullyellow $
      D.vcat $
        map (patternToDoc Unambiguous) unhandledPatterns

data Context
  = Arg
  | Unambiguous
  deriving (Eq)

patternToDoc :: Context -> P.Pattern -> D.Doc
patternToDoc context pattern =
  case pattern of
    P.Anything ->
      "_"
    P.Literal literal ->
      case literal of
        P.Chr chr ->
          "'" <> D.fromChars (ES.toChars chr) <> "'"
        P.Str str ->
          "\"" <> D.fromChars (ES.toChars str) <> "\""
        P.Int int ->
          D.fromInt int
    P.Ctor _ name args ->
      let ctorDoc =
            D.hsep (D.fromName name : map (patternToDoc Arg) args)
       in if context == Arg && length args > 0
            then "(" <> ctorDoc <> ")"
            else ctorDoc
    P.Array [] ->
      "[]"
    P.Array entries ->
      let entryDocs = map (patternToDoc Unambiguous) entries
       in "[ " <> D.hcat (List.intersperse ", " entryDocs) <> " ]"
    P.Record fields
      | Map.size fields == 0 ->
          "{}"
    P.Record fields ->
      let fieldDocs = map recordFieldToDoc (Map.toList fields)
       in "{ " <> D.hcat (List.intersperse ", " fieldDocs) <> " }"

recordFieldToDoc :: (Name.Name, P.Pattern) -> D.Doc
recordFieldToDoc (name, pattern) =
  D.fromName name <> " = " <> patternToDoc Unambiguous pattern
