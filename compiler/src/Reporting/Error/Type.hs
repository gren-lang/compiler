{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Reporting.Error.Type
  ( Error (..),
    -- expectations
    Expected (..),
    Context (..),
    SubContext (..),
    MaybeName (..),
    Category (..),
    PExpected (..),
    PContext (..),
    PCategory (..),
    typeReplace,
    ptypeReplace,
    -- make reports
    toReport,
  )
where

import AST.Canonical qualified as Can
import Data.Index qualified as Index
import Data.Map qualified as Map
import Data.Name qualified as Name
import Reporting.Annotation qualified as A
import Reporting.Doc qualified as D
import Reporting.Render.Code qualified as Code
import Reporting.Render.Type qualified as RT
import Reporting.Render.Type.Localizer qualified as L
import Reporting.Report qualified as Report
import Reporting.Suggest qualified as Suggest
import Type.Error qualified as T
import Prelude hiding (round)

-- ERRORS

data Error
  = BadExpr A.Region Category T.Type (Expected T.Type)
  | BadPattern A.Region PCategory T.Type (PExpected T.Type)
  | InfiniteType A.Region Name.Name T.Type

-- EXPRESSION EXPECTATIONS

data Expected tipe
  = NoExpectation tipe
  | FromContext A.Region Context tipe
  | FromAnnotation Name.Name Int SubContext tipe

data Context
  = ArrayEntry Index.ZeroBased
  | Negate
  | OpLeft Name.Name
  | OpRight Name.Name
  | IfCondition
  | IfBranch Index.ZeroBased
  | CaseBranch Index.ZeroBased
  | CallArity MaybeName Int
  | CallArg MaybeName Index.ZeroBased
  | RecordAccess A.Region (Maybe Name.Name) A.Region Name.Name
  | RecordUpdateKeys (Map.Map Name.Name Can.FieldUpdate)
  | RecordUpdateValue Name.Name
  | Destructure

data SubContext
  = TypedIfBranch Index.ZeroBased
  | TypedCaseBranch Index.ZeroBased
  | TypedBody

data MaybeName
  = FuncName Name.Name
  | CtorName Name.Name
  | OpName Name.Name
  | NoName

data Category
  = Array
  | Number
  | Float
  | String
  | Char
  | If
  | Case
  | CallResult MaybeName
  | Lambda
  | Accessor Name.Name
  | Access Name.Name
  | Record
  | Effects
  | Local Name.Name
  | Foreign Name.Name

-- PATTERN EXPECTATIONS

data PExpected tipe
  = PNoExpectation tipe
  | PFromContext A.Region PContext tipe

data PContext
  = PTypedArg Name.Name Index.ZeroBased
  | PCaseMatch Index.ZeroBased
  | PCtorArg Name.Name Index.ZeroBased
  | PArrayEntry Index.ZeroBased

data PCategory
  = PRecord
  | PArray
  | PCtor Name.Name
  | PInt
  | PStr
  | PChr
  | PBool

-- HELPERS

typeReplace :: Expected a -> b -> Expected b
typeReplace expectation tipe =
  case expectation of
    NoExpectation _ ->
      NoExpectation tipe
    FromContext region context _ ->
      FromContext region context tipe
    FromAnnotation name arity context _ ->
      FromAnnotation name arity context tipe

ptypeReplace :: PExpected a -> b -> PExpected b
ptypeReplace expectation tipe =
  case expectation of
    PNoExpectation _ ->
      PNoExpectation tipe
    PFromContext region context _ ->
      PFromContext region context tipe

-- TO REPORT

toReport :: Code.Source -> L.Localizer -> Error -> Report.Report
toReport source localizer err =
  case err of
    BadExpr region category actualType expected ->
      toExprReport source localizer region category actualType expected
    BadPattern region category tipe expected ->
      toPatternReport source localizer region category tipe expected
    InfiniteType region name overallType ->
      toInfiniteReport source localizer region name overallType

-- TO PATTERN REPORT

toPatternReport :: Code.Source -> L.Localizer -> A.Region -> PCategory -> T.Type -> PExpected T.Type -> Report.Report
toPatternReport source localizer patternRegion category tipe expected =
  Report.Report "TYPE MISMATCH" patternRegion [] $
    case expected of
      PNoExpectation expectedType ->
        Code.toSnippet source patternRegion Nothing $
          ( "This pattern is being used in an unexpected way:",
            patternTypeComparison
              localizer
              tipe
              expectedType
              (addPatternCategory "It is" category)
              "But it needs to match:"
              []
          )
      PFromContext region context expectedType ->
        Code.toSnippet source region (Just patternRegion) $
          case context of
            PTypedArg name index ->
              ( D.reflow $
                  "The " <> D.ordinal index <> " argument to `" <> Name.toChars name <> "` is weird.",
                patternTypeComparison
                  localizer
                  tipe
                  expectedType
                  (addPatternCategory "The argument is a pattern that matches" category)
                  ( "But the type annotation on `"
                      <> Name.toChars name
                      <> "` says the "
                      <> D.ordinal index
                      <> " argument should be:"
                  )
                  []
              )
            PCaseMatch index ->
              if index == Index.first
                then
                  ( D.reflow $
                      "The 1st pattern in this `case` causing a mismatch:",
                    patternTypeComparison
                      localizer
                      tipe
                      expectedType
                      (addPatternCategory "The first pattern is trying to match" category)
                      "But the expression between `case` and `of` is:"
                      [ D.reflow $
                          "These can never match! Is the pattern the problem? Or is it the expression?"
                      ]
                  )
                else
                  ( D.reflow $
                      "The " <> D.ordinal index <> " pattern in this `case` does not match the previous ones.",
                    patternTypeComparison
                      localizer
                      tipe
                      expectedType
                      (addPatternCategory ("The " <> D.ordinal index <> " pattern is trying to match") category)
                      "But all the previous patterns match:"
                      [ D.link
                          "Note"
                          "A `case` expression can only handle one type of value, so you may want to use"
                          "custom-types"
                          "to handle “mixing” types."
                      ]
                  )
            PCtorArg name index ->
              ( D.reflow $
                  "The " <> D.ordinal index <> " argument to `" <> Name.toChars name <> "` is weird.",
                patternTypeComparison
                  localizer
                  tipe
                  expectedType
                  (addPatternCategory "It is trying to match" category)
                  ( "But `"
                      <> Name.toChars name
                      <> "` needs its "
                      <> D.ordinal index
                      <> " argument to be:"
                  )
                  []
              )
            PArrayEntry index ->
              ( D.reflow $
                  "The " <> D.ordinal index <> " pattern in this array does not match all the previous ones:",
                patternTypeComparison
                  localizer
                  tipe
                  expectedType
                  (addPatternCategory ("The " <> D.ordinal index <> " pattern is trying to match") category)
                  "But all the previous patterns in the array are:"
                  [ D.link
                      "Hint"
                      "Everything in an array must be the same type of value. This way, we never\
                      \ run into unexpected values partway through a Array.map, Array.foldl, etc. Read"
                      "custom-types"
                      "to learn how to “mix” types."
                  ]
              )

-- PATTERN HELPERS

patternTypeComparison :: L.Localizer -> T.Type -> T.Type -> String -> String -> [D.Doc] -> D.Doc
patternTypeComparison localizer actual expected iAmSeeing insteadOf contextHints =
  let (actualDoc, expectedDoc, problems) =
        T.toComparison localizer actual expected
   in D.stack $
        [ D.reflow iAmSeeing,
          D.indent 4 actualDoc,
          D.reflow insteadOf,
          D.indent 4 expectedDoc
        ]
          ++ problemsToHint problems
          ++ contextHints

addPatternCategory :: String -> PCategory -> String
addPatternCategory iAmTryingToMatch category =
  iAmTryingToMatch
    <> case category of
      PRecord -> " record values of type:"
      PArray -> " arrays of type:"
      PCtor name -> " `" <> Name.toChars name <> "` values of type:"
      PInt -> " integers:"
      PStr -> " strings:"
      PChr -> " characters:"
      PBool -> " booleans:"

-- EXPR HELPERS

typeComparison :: L.Localizer -> T.Type -> T.Type -> String -> String -> [D.Doc] -> D.Doc
typeComparison localizer actual expected iAmSeeing insteadOf contextHints =
  let (actualDoc, expectedDoc, problems) =
        T.toComparison localizer actual expected
   in D.stack $
        [ D.reflow iAmSeeing,
          D.indent 4 actualDoc,
          D.reflow insteadOf,
          D.indent 4 expectedDoc
        ]
          ++ contextHints
          ++ problemsToHint problems

loneType :: L.Localizer -> T.Type -> T.Type -> D.Doc -> [D.Doc] -> D.Doc
loneType localizer actual expected iAmSeeing furtherDetails =
  let (actualDoc, _, problems) =
        T.toComparison localizer actual expected
   in D.stack $
        [ iAmSeeing,
          D.indent 4 actualDoc
        ]
          ++ furtherDetails
          ++ problemsToHint problems

addCategory :: String -> Category -> String
addCategory thisIs category =
  case category of
    Local name -> "This `" <> Name.toChars name <> "` value is a:"
    Foreign name -> "This `" <> Name.toChars name <> "` value is a:"
    Access field -> "The value at ." <> Name.toChars field <> " is a:"
    Accessor field -> "This ." <> Name.toChars field <> " field access function has type:"
    If -> "This `if` expression produces:"
    Case -> "This `case` expression produces:"
    Array -> thisIs <> " an array of type:"
    Number -> thisIs <> " a number of type:"
    Float -> thisIs <> " a float of type:"
    String -> thisIs <> " a string of type:"
    Char -> thisIs <> " a character of type:"
    Lambda -> thisIs <> " an anonymous function of type:"
    Record -> thisIs <> " a record of type:"
    Effects -> thisIs <> " a thing for CORE LIBRARIES ONLY."
    CallResult maybeName ->
      case maybeName of
        NoName -> thisIs <> ":"
        FuncName name -> "This `" <> Name.toChars name <> "` call produces:"
        CtorName name -> "This `" <> Name.toChars name <> "` call produces:"
        OpName _ -> thisIs <> ":"

problemsToHint :: [T.Problem] -> [D.Doc]
problemsToHint problems =
  case problems of
    [] ->
      []
    problem : _ ->
      problemToHint problem

problemToHint :: T.Problem -> [D.Doc]
problemToHint problem =
  case problem of
    T.IntFloat ->
      [ D.fancyLink
          "Note"
          ["Read"]
          "implicit-casts"
          [ "to",
            "learn",
            "why",
            "Gren",
            "does",
            "not",
            "implicitly",
            "convert",
            "Ints",
            "to",
            "Floats.",
            "Use",
            D.green "toFloat",
            "and",
            D.green "round",
            "to",
            "do",
            "explicit",
            "conversions."
          ]
      ]
    T.StringFromInt ->
      [ D.toFancyHint
          [ "Want",
            "to",
            "convert",
            "an",
            "Int",
            "into",
            "a",
            "String?",
            "Use",
            "the",
            D.green "String.fromInt",
            "function!"
          ]
      ]
    T.StringFromFloat ->
      [ D.toFancyHint
          [ "Want",
            "to",
            "convert",
            "a",
            "Float",
            "into",
            "a",
            "String?",
            "Use",
            "the",
            D.green "String.fromFloat",
            "function!"
          ]
      ]
    T.StringToInt ->
      [ D.toFancyHint
          [ "Want",
            "to",
            "convert",
            "a",
            "String",
            "into",
            "an",
            "Int?",
            "Use",
            "the",
            D.green "String.toInt",
            "function!"
          ]
      ]
    T.StringToFloat ->
      [ D.toFancyHint
          [ "Want",
            "to",
            "convert",
            "a",
            "String",
            "into",
            "a",
            "Float?",
            "Use",
            "the",
            D.green "String.toFloat",
            "function!"
          ]
      ]
    T.AnythingToBool ->
      [ D.toSimpleHint $
          "Gren does not have “truthiness” such that ints and strings and arrays\
          \ are automatically converted to booleans. Do that conversion explicitly!"
      ]
    T.AnythingFromMaybe ->
      [ D.toFancyHint
          [ "Use",
            D.green "Maybe.withDefault",
            "to",
            "handle",
            "possible",
            "errors.",
            "Longer",
            "term,",
            "it",
            "is",
            "usually",
            "better",
            "to",
            "write",
            "out",
            "the",
            "full",
            "`case`",
            "though!"
          ]
      ]
    T.ArityMismatch x y ->
      [ D.toSimpleHint $
          if x < y
            then "It looks like it takes too few arguments. I was expecting " ++ show (y - x) ++ " more."
            else "It looks like it takes too many arguments. I see " ++ show (x - y) ++ " extra."
      ]
    T.BadFlexSuper direction super _ tipe ->
      case tipe of
        T.Lambda _ _ _ -> badFlexSuper direction super tipe
        T.Infinite -> []
        T.Error -> []
        T.FlexVar _ -> []
        T.FlexSuper s _ -> badFlexFlexSuper super s
        T.RigidVar y -> badRigidVar y (toASuperThing super)
        T.RigidSuper s _ -> badRigidSuper s (toASuperThing super)
        T.Type _ _ _ -> badFlexSuper direction super tipe
        T.Record _ _ -> badFlexSuper direction super tipe
        T.Alias _ _ _ _ -> badFlexSuper direction super tipe
    T.BadRigidVar x tipe ->
      case tipe of
        T.Lambda _ _ _ -> badRigidVar x "a function"
        T.Infinite -> []
        T.Error -> []
        T.FlexVar _ -> []
        T.FlexSuper s _ -> badRigidVar x (toASuperThing s)
        T.RigidVar y -> badDoubleRigid x y
        T.RigidSuper _ y -> badDoubleRigid x y
        T.Type _ n _ -> badRigidVar x ("a `" ++ Name.toChars n ++ "` value")
        T.Record _ _ -> badRigidVar x "a record"
        T.Alias _ n _ _ -> badRigidVar x ("a `" ++ Name.toChars n ++ "` value")
    T.BadRigidSuper super x tipe ->
      case tipe of
        T.Lambda _ _ _ -> badRigidSuper super "a function"
        T.Infinite -> []
        T.Error -> []
        T.FlexVar _ -> []
        T.FlexSuper s _ -> badRigidSuper super (toASuperThing s)
        T.RigidVar y -> badDoubleRigid x y
        T.RigidSuper _ y -> badDoubleRigid x y
        T.Type _ n _ -> badRigidSuper super ("a `" ++ Name.toChars n ++ "` value")
        T.Record _ _ -> badRigidSuper super "a record"
        T.Alias _ n _ _ -> badRigidSuper super ("a `" ++ Name.toChars n ++ "` value")
    T.FieldsMissing fields ->
      case map (D.green . D.fromName) fields of
        [] ->
          []
        [f1] ->
          [ D.toFancyHint ["Looks", "like", "the", f1, "field", "is", "missing."]
          ]
        fieldDocs ->
          [ D.toFancyHint $
              ["Looks", "like", "fields"] ++ D.commaSep "and" id fieldDocs ++ ["are", "missing."]
          ]
    T.FieldTypo typo possibilities ->
      case Suggest.sort (Name.toChars typo) Name.toChars possibilities of
        [] ->
          []
        nearest : _ ->
          [ D.toFancyHint $
              [ "Seems",
                "like",
                "a",
                "record",
                "field",
                "typo.",
                "Maybe",
                D.dullyellow (D.fromName typo),
                "should",
                "be",
                D.green (D.fromName nearest) <> "?"
              ],
            D.toSimpleHint
              "Can more type annotations be added? Type annotations always help me give\
              \ more specific messages, and I think they could help a lot in this case!"
          ]

-- BAD RIGID HINTS

badRigidVar :: Name.Name -> String -> [D.Doc]
badRigidVar name aThing =
  [ D.toSimpleHint $
      "Your type annotation uses type variable `"
        ++ Name.toChars name
        ++ "` which means ANY type of value can flow through, but your code is saying it specifically wants "
        ++ aThing
        ++ ". Maybe change your type annotation to\
           \ be more specific? Maybe change the code to be more general?",
    D.reflowLink "Read" "type-annotations" "for more advice!"
  ]

badDoubleRigid :: Name.Name -> Name.Name -> [D.Doc]
badDoubleRigid x y =
  [ D.toSimpleHint $
      "Your type annotation uses `"
        ++ Name.toChars x
        ++ "` and `"
        ++ Name.toChars y
        ++ "` as separate type variables. Your code seems to be saying they are the\
           \ same though. Maybe they should be the same in your type annotation?\
           \ Maybe your code uses them in a weird way?",
    D.reflowLink "Read" "type-annotations" "for more advice!"
  ]

toASuperThing :: T.Super -> String
toASuperThing super =
  case super of
    T.Number -> "a `number` value"
    T.Comparable -> "a `comparable` value"
    T.CompAppend -> "a `compappend` value"
    T.Appendable -> "an `appendable` value"

-- BAD SUPER HINTS

badFlexSuper :: T.Direction -> T.Super -> T.Type -> [D.Doc]
badFlexSuper direction super tipe =
  case super of
    T.Comparable ->
      case tipe of
        T.Record _ _ ->
          [ D.link
              "Hint"
              "I do not know how to compare records. I can only compare ints, floats,\
              \ chars, strings and arrays of comparable values.\
              \ Check out"
              "comparing-records"
              "for ideas on how to proceed."
          ]
        T.Type _ name _ ->
          [ D.toSimpleHint $
              "I do not know how to compare `"
                ++ Name.toChars name
                ++ "` values. I can only\
                   \ compare ints, floats, chars, strings and arrays of comparable values.",
            D.reflowLink
              "Check out"
              "comparing-custom-types"
              "for ideas on how to proceed."
          ]
        _ ->
          [ D.toSimpleHint $
              "I only know how to compare ints, floats, chars, strings and arrays of comparable values."
          ]
    T.Appendable ->
      [ D.toSimpleHint "I only know how to append strings and arrays."
      ]
    T.CompAppend ->
      [ D.toSimpleHint "Only strings and arrays are both comparable and appendable."
      ]
    T.Number ->
      case tipe of
        T.Type home name _ | T.isString home name ->
          case direction of
            T.Have ->
              [ D.toFancyHint ["Try", "using", D.green "String.fromInt", "to", "convert", "it", "to", "a", "string?"]
              ]
            T.Need ->
              [ D.toFancyHint ["Try", "using", D.green "String.toInt", "to", "convert", "it", "to", "an", "integer?"]
              ]
        _ ->
          [ D.toFancyHint ["Only", D.green "Int", "and", D.green "Float", "values", "work", "as", "numbers."]
          ]

badRigidSuper :: T.Super -> String -> [D.Doc]
badRigidSuper super aThing =
  let (superType, manyThings) =
        case super of
          T.Number -> ("number", "ints AND floats")
          T.Comparable -> ("comparable", "ints, floats, chars, strings and arrays")
          T.Appendable -> ("appendable", "strings AND arrays")
          T.CompAppend -> ("compappend", "strings AND arrays")
   in [ D.toSimpleHint $
          "The `"
            ++ superType
            ++ "` in your type annotation is saying that "
            ++ manyThings
            ++ " can flow through, but your code is saying it specifically wants "
            ++ aThing
            ++ ". Maybe change your type annotation to\
               \ be more specific? Maybe change the code to be more general?",
        D.reflowLink "Read" "type-annotations" "for more advice!"
      ]

badFlexFlexSuper :: T.Super -> T.Super -> [D.Doc]
badFlexFlexSuper s1 s2 =
  let likeThis super =
        case super of
          T.Number -> "a number"
          T.Comparable -> "comparable"
          T.CompAppend -> "a compappend"
          T.Appendable -> "appendable"
   in [ D.toSimpleHint $
          "There are no values in Gren that are both "
            ++ likeThis s1
            ++ " and "
            ++ likeThis s2
            ++ "."
      ]

-- TO EXPR REPORT

toExprReport :: Code.Source -> L.Localizer -> A.Region -> Category -> T.Type -> Expected T.Type -> Report.Report
toExprReport source localizer exprRegion category tipe expected =
  case expected of
    NoExpectation expectedType ->
      Report.Report "TYPE MISMATCH" exprRegion [] $
        Code.toSnippet
          source
          exprRegion
          Nothing
          ( "This expression is being used in an unexpected way:",
            typeComparison
              localizer
              tipe
              expectedType
              (addCategory "It is" category)
              "But you are trying to use it as:"
              []
          )
    FromAnnotation name _arity subContext expectedType ->
      let thing =
            case subContext of
              TypedIfBranch index -> D.ordinal index <> " branch of this `if` expression:"
              TypedCaseBranch index -> D.ordinal index <> " branch of this `case` expression:"
              TypedBody -> "body of the `" <> Name.toChars name <> "` definition:"

          itIs =
            case subContext of
              TypedIfBranch index -> "The " <> D.ordinal index <> " branch is"
              TypedCaseBranch index -> "The " <> D.ordinal index <> " branch is"
              TypedBody -> "The body is"
       in Report.Report "TYPE MISMATCH" exprRegion [] $
            Code.toSnippet source exprRegion Nothing $
              ( D.reflow ("Something is off with the " <> thing),
                typeComparison
                  localizer
                  tipe
                  expectedType
                  (addCategory itIs category)
                  ("But the type annotation on `" <> Name.toChars name <> "` says it should be:")
                  []
              )
    FromContext region context expectedType ->
      let mismatch (maybeHighlight, problem, thisIs, insteadOf, furtherDetails) =
            Report.Report "TYPE MISMATCH" exprRegion [] $
              Code.toSnippet
                source
                region
                maybeHighlight
                ( D.reflow problem,
                  typeComparison localizer tipe expectedType (addCategory thisIs category) insteadOf furtherDetails
                )

          badType (maybeHighlight, problem, thisIs, furtherDetails) =
            Report.Report "TYPE MISMATCH" exprRegion [] $
              Code.toSnippet
                source
                region
                maybeHighlight
                ( D.reflow problem,
                  loneType localizer tipe expectedType (D.reflow (addCategory thisIs category)) furtherDetails
                )

          custom maybeHighlight docPair =
            Report.Report "TYPE MISMATCH" exprRegion [] $
              Code.toSnippet source region maybeHighlight docPair
       in case context of
            ArrayEntry index ->
              let ith = D.ordinal index
               in mismatch
                    ( Just exprRegion,
                      "The " <> ith <> " element of this array does not match all the previous elements:",
                      "The " <> ith <> " element is",
                      "But all the previous elements in the array are:",
                      [ D.link
                          "Hint"
                          "Everything in a array must be the same type of value. This way, we never\
                          \ run into unexpected values partway through a Array.map, Array.foldl, etc. Read"
                          "custom-types"
                          "to learn how to “mix” types."
                      ]
                    )
            Negate ->
              badType
                ( Just exprRegion,
                  "I do not know how to negate this type of value:",
                  "It is",
                  [ D.fillSep
                      [ "But",
                        "I",
                        "only",
                        "now",
                        "how",
                        "to",
                        "negate",
                        D.dullyellow "Int",
                        "and",
                        D.dullyellow "Float",
                        "values."
                      ]
                  ]
                )
            OpLeft op ->
              custom (Just exprRegion) $
                opLeftToDocs localizer category op tipe expectedType
            OpRight op ->
              case opRightToDocs localizer category op tipe expectedType of
                EmphBoth details ->
                  custom Nothing details
                EmphRight details ->
                  custom (Just exprRegion) details
            IfCondition ->
              badType
                ( Just exprRegion,
                  "This `if` condition does not evaluate to a boolean value, True or False.",
                  "It is",
                  [ D.fillSep ["But", "I", "need", "this", "`if`", "condition", "to", "be", "a", D.dullyellow "Bool", "value."]
                  ]
                )
            IfBranch index ->
              let ith = D.ordinal index
               in mismatch
                    ( Just exprRegion,
                      "The " <> ith <> " branch of this `if` does not match all the previous branches:",
                      "The " <> ith <> " branch is",
                      "But all the previous branches result in:",
                      [ D.link
                          "Hint"
                          "All branches in an `if` must produce the same type of values. This way, no\
                          \ matter which branch we take, the result is always a consistent shape. Read"
                          "custom-types"
                          "to learn how to “mix” types."
                      ]
                    )
            CaseBranch index ->
              let ith = D.ordinal index
               in mismatch
                    ( Just exprRegion,
                      "The " <> ith <> " branch of this `case` does not match all the previous branches:",
                      "The " <> ith <> " branch is",
                      "But all the previous branches result in:",
                      [ D.link
                          "Hint"
                          "All branches in a `case` must produce the same type of values. This way, no\
                          \ matter which branch we take, the result is always a consistent shape. Read"
                          "custom-types"
                          "to learn how to “mix” types."
                      ]
                    )
            CallArity maybeFuncName numGivenArgs ->
              Report.Report "TOO MANY ARGS" exprRegion [] $
                Code.toSnippet source region (Just exprRegion) $
                  case countArgs tipe of
                    0 ->
                      let thisValue =
                            case maybeFuncName of
                              NoName -> "This value"
                              FuncName name -> "The `" <> Name.toChars name <> "` value"
                              CtorName name -> "The `" <> Name.toChars name <> "` value"
                              OpName op -> "The (" <> Name.toChars op <> ") operator"
                       in ( D.reflow $ thisValue <> " is not a function, but it was given " <> D.args numGivenArgs <> ".",
                            D.reflow $ "Are there any missing commas? Or missing parentheses?"
                          )
                    n ->
                      let thisFunction =
                            case maybeFuncName of
                              NoName -> "This function"
                              FuncName name -> "The `" <> Name.toChars name <> "` function"
                              CtorName name -> "The `" <> Name.toChars name <> "` constructor"
                              OpName op -> "The (" <> Name.toChars op <> ") operator"
                       in ( D.reflow $ thisFunction <> " expects " <> D.args n <> ", but it got " <> show numGivenArgs <> " instead.",
                            D.reflow $ "Are there any missing commas? Or missing parentheses?"
                          )
            CallArg maybeFuncName index ->
              let ith = D.ordinal index

                  thisFunction =
                    case maybeFuncName of
                      NoName -> "this function"
                      FuncName name -> "`" <> Name.toChars name <> "`"
                      CtorName name -> "`" <> Name.toChars name <> "`"
                      OpName op -> "(" <> Name.toChars op <> ")"
               in mismatch
                    ( Just exprRegion,
                      "The " <> ith <> " argument to " <> thisFunction <> " is not what I expect:",
                      "This argument is",
                      "But " <> thisFunction <> " needs the " <> ith <> " argument to be:",
                      if Index.toHuman index == 1
                        then []
                        else
                          [ D.toSimpleHint $
                              "I always figure out the argument types from left to right. If an argument\
                              \ is acceptable, I assume it is “correct” and move on. So the problem may\
                              \ actually be in one of the previous arguments!"
                          ]
                    )
            RecordAccess recordRegion maybeName fieldRegion field ->
              case T.iteratedDealias tipe of
                T.Record fields ext ->
                  custom
                    (Just fieldRegion)
                    ( D.reflow $
                        "This "
                          <> maybe "" (\n -> "`" <> Name.toChars n <> "`") maybeName
                          <> " record does not have a `"
                          <> Name.toChars field
                          <> "` field:",
                      case Suggest.sort (Name.toChars field) (Name.toChars . fst) (Map.toList fields) of
                        [] ->
                          D.reflow "In fact, it is a record with NO fields!"
                        f : fs ->
                          D.stack
                            [ D.reflow $
                                "This is usually a typo. Here are the "
                                  <> maybe "" (\n -> "`" <> Name.toChars n <> "`") maybeName
                                  <> " fields that are most similar:",
                              toNearbyRecord localizer f fs ext,
                              D.fillSep
                                [ "So",
                                  "maybe",
                                  D.dullyellow (D.fromName field),
                                  "should",
                                  "be",
                                  D.green (D.fromName (fst f)) <> "?"
                                ]
                            ]
                    )
                _ ->
                  badType
                    ( Just recordRegion,
                      "This is not a record, so it has no fields to access!",
                      "It is",
                      [ D.fillSep
                          [ "But",
                            "I",
                            "need",
                            "a",
                            "record",
                            "with",
                            "a",
                            D.dullyellow (D.fromName field),
                            "field!"
                          ]
                      ]
                    )
            RecordUpdateKeys expectedFields ->
              case T.iteratedDealias tipe of
                T.Record actualFields ext ->
                  case Map.lookupMin (Map.difference expectedFields actualFields) of
                    Nothing ->
                      mismatch
                        ( Nothing,
                          "Something is off with this record update:",
                          "The record is",
                          "But this update needs it to be compatable with:",
                          [ D.reflow
                              "Do you mind creating an <http://sscce.org/> that produces this error message and\
                              \ sharing it at <https://github.com/gren/error-message-catalog/issues> so we\
                              \ can try to give better advice here?"
                          ]
                        )
                    Just (field, Can.FieldUpdate fieldRegion _) ->
                      let fStr = "`" <> Name.toChars field <> "`"
                       in custom
                            (Just fieldRegion)
                            ( D.reflow $
                                "The record does not have a " <> fStr <> " field:",
                              case Suggest.sort (Name.toChars field) (Name.toChars . fst) (Map.toList actualFields) of
                                [] ->
                                  D.reflow $ "In fact, this is a record with NO fields!"
                                f : fs ->
                                  D.stack
                                    [ D.reflow $
                                        "This is usually a typo. Here are the fields that are most similar:",
                                      toNearbyRecord localizer f fs ext,
                                      D.fillSep
                                        [ "So",
                                          "maybe",
                                          D.dullyellow (D.fromName field),
                                          "should",
                                          "be",
                                          D.green (D.fromName (fst f)) <> "?"
                                        ]
                                    ]
                            )
                _ ->
                  badType
                    ( Just exprRegion,
                      "This is not a record, so it has no fields to update!",
                      "It is",
                      [ D.reflow $ "But I need a record!"
                      ]
                    )
            RecordUpdateValue field ->
              mismatch
                ( Just exprRegion,
                  "I cannot update the `" <> Name.toChars field <> "` field like this:",
                  "You are trying to update `" <> Name.toChars field <> "` to be",
                  "But it should be:",
                  [ D.toSimpleNote
                      "The record update syntax does not allow you to change the type of fields.\
                      \ You can achieve that with record constructors or the record literal syntax."
                  ]
                )
            Destructure ->
              mismatch
                ( Nothing,
                  "This definition is causing issues:",
                  "You are defining",
                  "But then trying to destructure it as:",
                  []
                )

-- HELPERS

countArgs :: T.Type -> Int
countArgs tipe =
  case tipe of
    T.Lambda _ _ stuff ->
      1 + length stuff
    _ ->
      0

-- FIELD NAME HELPERS

toNearbyRecord :: L.Localizer -> (Name.Name, T.Type) -> [(Name.Name, T.Type)] -> T.Extension -> D.Doc
toNearbyRecord localizer f fs ext =
  D.indent 4 $
    if length fs <= 3
      then RT.vrecord (map (fieldToDocs localizer) (f : fs)) (extToDoc ext)
      else RT.vrecordSnippet (fieldToDocs localizer f) (map (fieldToDocs localizer) (take 3 fs))

fieldToDocs :: L.Localizer -> (Name.Name, T.Type) -> (D.Doc, D.Doc)
fieldToDocs localizer (name, tipe) =
  ( D.fromName name,
    T.toDoc localizer RT.None tipe
  )

extToDoc :: T.Extension -> Maybe D.Doc
extToDoc ext =
  case ext of
    T.Closed -> Nothing
    T.FlexOpen x -> Just (D.fromName x)
    T.RigidOpen x -> Just (D.fromName x)

-- OP LEFT

opLeftToDocs :: L.Localizer -> Category -> Name.Name -> T.Type -> T.Type -> (D.Doc, D.Doc)
opLeftToDocs localizer category op tipe expected =
  case op of
    "+"
      | isString tipe -> badStringAdd
      | isArray tipe -> badArrayAdd localizer category "left" tipe expected
      | otherwise -> badMath localizer category "Addition" "left" "+" tipe expected []
    "*"
      | isArray tipe -> badArrayMul localizer category "left" tipe expected
      | otherwise -> badMath localizer category "Multiplication" "left" "*" tipe expected []
    "-" -> badMath localizer category "Subtraction" "left" "-" tipe expected []
    "^" -> badMath localizer category "Exponentiation" "left" "^" tipe expected []
    "/" -> badFDiv localizer "left" tipe expected
    "//" -> badIDiv localizer "left" tipe expected
    "&&" -> badBool localizer "&&" "left" tipe expected
    "||" -> badBool localizer "||" "left" tipe expected
    "<" -> badCompLeft localizer category "<" "left" tipe expected
    ">" -> badCompLeft localizer category ">" "left" tipe expected
    "<=" -> badCompLeft localizer category "<=" "left" tipe expected
    ">=" -> badCompLeft localizer category ">=" "left" tipe expected
    "++" -> badAppendLeft localizer category tipe expected
    "<|" ->
      ( "The left side of (<|) needs to be a function so I can pipe arguments to it!",
        loneType
          localizer
          tipe
          expected
          (D.reflow (addCategory "I am seeing" category))
          [ D.reflow $ "This needs to be some kind of function though!"
          ]
      )
    _ ->
      ( D.reflow $
          "The left argument of (" <> Name.toChars op <> ") is causing problems:",
        typeComparison
          localizer
          tipe
          expected
          (addCategory "The left argument is" category)
          ("But (" <> Name.toChars op <> ") needs the left argument to be:")
          []
      )

-- OP RIGHT

data RightDocs
  = EmphBoth (D.Doc, D.Doc)
  | EmphRight (D.Doc, D.Doc)

opRightToDocs :: L.Localizer -> Category -> Name.Name -> T.Type -> T.Type -> RightDocs
opRightToDocs localizer category op tipe expected =
  case op of
    "+"
      | isFloat expected && isInt tipe -> badCast op FloatInt
      | isInt expected && isFloat tipe -> badCast op IntFloat
      | isString tipe -> EmphRight $ badStringAdd
      | isArray tipe -> EmphRight $ badArrayAdd localizer category "right" tipe expected
      | otherwise -> EmphRight $ badMath localizer category "Addition" "right" "+" tipe expected []
    "*"
      | isFloat expected && isInt tipe -> badCast op FloatInt
      | isInt expected && isFloat tipe -> badCast op IntFloat
      | isArray tipe -> EmphRight $ badArrayMul localizer category "right" tipe expected
      | otherwise -> EmphRight $ badMath localizer category "Multiplication" "right" "*" tipe expected []
    "-"
      | isFloat expected && isInt tipe -> badCast op FloatInt
      | isInt expected && isFloat tipe -> badCast op IntFloat
      | otherwise ->
          EmphRight $ badMath localizer category "Subtraction" "right" "-" tipe expected []
    "^"
      | isFloat expected && isInt tipe -> badCast op FloatInt
      | isInt expected && isFloat tipe -> badCast op IntFloat
      | otherwise ->
          EmphRight $ badMath localizer category "Exponentiation" "right" "^" tipe expected []
    "/" -> EmphRight $ badFDiv localizer "right" tipe expected
    "//" -> EmphRight $ badIDiv localizer "right" tipe expected
    "&&" -> EmphRight $ badBool localizer "&&" "right" tipe expected
    "||" -> EmphRight $ badBool localizer "||" "right" tipe expected
    "<" -> badCompRight localizer "<" tipe expected
    ">" -> badCompRight localizer ">" tipe expected
    "<=" -> badCompRight localizer "<=" tipe expected
    ">=" -> badCompRight localizer ">=" tipe expected
    "==" -> badEquality localizer "==" tipe expected
    "/=" -> badEquality localizer "/=" tipe expected
    "++" -> badAppendRight localizer category tipe expected
    "<|" ->
      EmphRight
        ( D.reflow $ "I cannot send this through the (<|) pipe:",
          typeComparison
            localizer
            tipe
            expected
            "The argument is:"
            "But (<|) is piping it to a function that expects:"
            []
        )
    "|>" ->
      case (tipe, expected) of
        (T.Lambda expectedArgType _ _, T.Lambda argType _ _) ->
          EmphRight
            ( D.reflow $ "This function cannot handle the argument sent through the (|>) pipe:",
              typeComparison
                localizer
                argType
                expectedArgType
                "The argument is:"
                "But (|>) is piping it to a function that expects:"
                []
            )
        _ ->
          EmphRight
            ( D.reflow $ "The right side of (|>) needs to be a function so I can pipe arguments to it!",
              loneType
                localizer
                tipe
                expected
                (D.reflow (addCategory "But instead of a function, I am seeing" category))
                []
            )
    _ ->
      badOpRightFallback localizer category op tipe expected

badOpRightFallback :: L.Localizer -> Category -> Name.Name -> T.Type -> T.Type -> RightDocs
badOpRightFallback localizer category op tipe expected =
  EmphRight
    ( D.reflow $
        "The right argument of (" <> Name.toChars op <> ") is causing problems.",
      typeComparison
        localizer
        tipe
        expected
        (addCategory "The right argument is" category)
        ("But (" <> Name.toChars op <> ") needs the right argument to be:")
        [ D.toSimpleHint $
            "With operators like ("
              ++ Name.toChars op
              ++ ") I always check the left\
                 \ side first. If it seems fine, I assume it is correct and check the right\
                 \ side. So the problem may be in how the left and right arguments interact!"
        ]
    )

isInt :: T.Type -> Bool
isInt tipe =
  case tipe of
    T.Type home name [] ->
      T.isInt home name
    _ ->
      False

isFloat :: T.Type -> Bool
isFloat tipe =
  case tipe of
    T.Type home name [] ->
      T.isFloat home name
    _ ->
      False

isString :: T.Type -> Bool
isString tipe =
  case tipe of
    T.Type home name [] ->
      T.isString home name
    _ ->
      False

isArray :: T.Type -> Bool
isArray tipe =
  case tipe of
    T.Type home name [_] ->
      T.isArray home name
    _ ->
      False

-- BAD APPEND

data AppendType
  = ANumber D.Doc D.Doc
  | AString
  | AArray
  | AOther

toAppendType :: T.Type -> AppendType
toAppendType tipe =
  case tipe of
    T.Type home name _
      | T.isInt home name -> ANumber "Int" "String.fromInt"
      | T.isFloat home name -> ANumber "Float" "String.fromFloat"
      | T.isString home name -> AString
      | T.isArray home name -> AArray
    T.FlexSuper T.Number _ -> ANumber "number" "String.fromInt"
    _ -> AOther

badAppendLeft :: L.Localizer -> Category -> T.Type -> T.Type -> (D.Doc, D.Doc)
badAppendLeft localizer category tipe expected =
  case toAppendType tipe of
    ANumber thing stringFromThing ->
      ( D.fillSep
          [ "The",
            "(++)",
            "operator",
            "can",
            "append",
            "Array",
            "and",
            "String",
            "values,",
            "but",
            "not",
            D.dullyellow thing,
            "values",
            "like",
            "this:"
          ],
        D.fillSep
          [ "Try",
            "using",
            D.green stringFromThing,
            "to",
            "turn",
            "it",
            "into",
            "a",
            "String?",
            "Or",
            "put",
            "it",
            "in",
            "[]",
            "to",
            "make",
            "it",
            "an",
            "array?"
          ]
      )
    _ ->
      ( D.reflow $
          "The (++) operator cannot append this type of value:",
        loneType
          localizer
          tipe
          expected
          (D.reflow (addCategory "I am seeing" category))
          [ D.fillSep
              [ "But",
                "the",
                "(++)",
                "operator",
                "is",
                "only",
                "for",
                "appending",
                D.dullyellow "Array",
                "and",
                D.dullyellow "String",
                "values.",
                "Maybe",
                "put",
                "this",
                "value",
                "in",
                "[]",
                "to",
                "make",
                "it",
                "an",
                "array?"
              ]
          ]
      )

badAppendRight :: L.Localizer -> Category -> T.Type -> T.Type -> RightDocs
badAppendRight localizer category tipe expected =
  case (toAppendType expected, toAppendType tipe) of
    (AString, ANumber thing stringFromThing) ->
      EmphRight
        ( D.fillSep
            [ "I",
              "thought",
              "I",
              "was",
              "appending",
              D.dullyellow "String",
              "values",
              "here,",
              "not",
              D.dullyellow thing,
              "values",
              "like",
              "this:"
            ],
          D.fillSep
            ["Try", "using", D.green stringFromThing, "to", "turn", "it", "into", "a", "string?"]
        )
    (AArray, ANumber thing _) ->
      EmphRight
        ( D.fillSep
            [ "I",
              "thought",
              "I",
              "was",
              "appending",
              D.dullyellow "Array",
              "values",
              "here,",
              "not",
              D.dullyellow thing,
              "values",
              "like",
              "this:"
            ],
          D.reflow "Try putting it in [] to make it an array?"
        )
    (AString, AArray) ->
      EmphBoth
        ( D.reflow $
            "The (++) operator needs the same type of value on both sides:",
          D.fillSep
            [ "I",
              "see",
              "a",
              D.dullyellow "String",
              "on",
              "the",
              "left",
              "and",
              "a",
              D.dullyellow "Array",
              "on",
              "the",
              "right.",
              "Which",
              "should",
              "it",
              "be?",
              "Does",
              "the",
              "string",
              "need",
              "[]",
              "around",
              "it",
              "to",
              "become",
              "an",
              "array?"
            ]
        )
    (AArray, AString) ->
      EmphBoth
        ( D.reflow $
            "The (++) operator needs the same type of value on both sides:",
          D.fillSep
            [ "I",
              "see",
              "an",
              D.dullyellow "Array",
              "on",
              "the",
              "left",
              "and",
              "a",
              D.dullyellow "String",
              "on",
              "the",
              "right.",
              "Which",
              "should",
              "it",
              "be?",
              "Does",
              "the",
              "string",
              "need",
              "[]",
              "around",
              "it",
              "to",
              "become",
              "an",
              "array?"
            ]
        )
    (_, _) ->
      EmphBoth
        ( D.reflow $
            "The (++) operator cannot append these two values:",
          typeComparison
            localizer
            expected
            tipe
            "I already figured out that the left side of (++) is:"
            (addCategory "But this clashes with the right side, which is" category)
            []
        )

-- BAD MATH

data ThisThenThat = FloatInt | IntFloat

badCast :: Name.Name -> ThisThenThat -> RightDocs
badCast op thisThenThat =
  EmphBoth
    ( D.reflow $
        "I need both sides of (" <> Name.toChars op <> ") to be the exact same type. Both Int or both Float.",
      let anInt = ["an", D.dullyellow "Int"]
          aFloat = ["a", D.dullyellow "Float"]
          toFloat = D.green "toFloat"
          round = D.green "round"
       in case thisThenThat of
            FloatInt ->
              badCastHelp aFloat anInt round toFloat
            IntFloat ->
              badCastHelp anInt aFloat toFloat round
    )

badCastHelp :: [D.Doc] -> [D.Doc] -> D.Doc -> D.Doc -> D.Doc
badCastHelp anInt aFloat toFloat round =
  D.stack
    [ D.fillSep $
        ["But", "I", "see"]
          ++ anInt
          ++ ["on", "the", "left", "and"]
          ++ aFloat
          ++ ["on", "the", "right."],
      D.fillSep
        [ "Use",
          toFloat,
          "on",
          "the",
          "left",
          "(or",
          round,
          "on",
          "the",
          "right)",
          "to",
          "make",
          "both",
          "sides",
          "match!"
        ],
      D.link "Note" "Read" "implicit-casts" "to learn why Gren does not implicitly convert Ints to Floats."
    ]

badStringAdd :: (D.Doc, D.Doc)
badStringAdd =
  ( D.fillSep ["I", "cannot", "do", "addition", "with", D.dullyellow "String", "values", "like", "this", "one:"],
    D.stack
      [ D.fillSep
          [ "The",
            "(+)",
            "operator",
            "only",
            "works",
            "with",
            D.dullyellow "Int",
            "and",
            D.dullyellow "Float",
            "values."
          ],
        D.toFancyHint
          [ "Switch",
            "to",
            "the",
            D.green "(++)",
            "operator",
            "to",
            "append",
            "strings!"
          ]
      ]
  )

badArrayAdd :: L.Localizer -> Category -> String -> T.Type -> T.Type -> (D.Doc, D.Doc)
badArrayAdd localizer category direction tipe expected =
  ( "I cannot do addition with array:",
    loneType
      localizer
      tipe
      expected
      (D.reflow (addCategory ("The " <> direction <> " side of (+) is") category))
      [ D.fillSep
          [ "But",
            "(+)",
            "only",
            "works",
            "with",
            D.dullyellow "Int",
            "and",
            D.dullyellow "Float",
            "values."
          ],
        D.toFancyHint
          [ "Switch",
            "to",
            "the",
            D.green "(++)",
            "operator",
            "to",
            "append",
            "arrays!"
          ]
      ]
  )

badArrayMul :: L.Localizer -> Category -> String -> T.Type -> T.Type -> (D.Doc, D.Doc)
badArrayMul localizer category direction tipe expected =
  badMath
    localizer
    category
    "Multiplication"
    direction
    "*"
    tipe
    expected
    [ D.toFancyHint
        [ "Maybe",
          "you",
          "want",
          D.green "Array.repeat",
          "to",
          "build",
          "an",
          "array",
          "of",
          "repeated",
          "values?"
        ]
    ]

badMath :: L.Localizer -> Category -> String -> String -> String -> T.Type -> T.Type -> [D.Doc] -> (D.Doc, D.Doc)
badMath localizer category operation direction op tipe expected otherHints =
  ( D.reflow $
      operation ++ " does not work with this value:",
    loneType
      localizer
      tipe
      expected
      (D.reflow (addCategory ("The " <> direction <> " side of (" <> op <> ") is") category))
      ( [ D.fillSep
            [ "But",
              "(" <> D.fromChars op <> ")",
              "only",
              "works",
              "with",
              D.dullyellow "Int",
              "and",
              D.dullyellow "Float",
              "values."
            ]
        ]
          ++ otherHints
      )
  )

badFDiv :: L.Localizer -> D.Doc -> T.Type -> T.Type -> (D.Doc, D.Doc)
badFDiv localizer direction tipe expected =
  ( D.reflow $
      "The (/) operator is specifically for floating-point division:",
    if isInt tipe
      then
        D.stack
          [ D.fillSep
              [ "The",
                direction,
                "side",
                "of",
                "(/)",
                "must",
                "be",
                "a",
                D.dullyellow "Float" <> ",",
                "but",
                "I",
                "am",
                "seeing",
                "an",
                D.dullyellow "Int" <> ".",
                "I",
                "recommend:"
              ],
            D.vcat
              [ D.green "toFloat" <> " for explicit conversions     " <> D.black "(toFloat 5 / 2) == 2.5",
                D.green "(//)   " <> " for integer division         " <> D.black "(5 // 2)        == 2"
              ],
            D.link "Note" "Read" "implicit-casts" "to learn why Gren does not implicitly convert Ints to Floats."
          ]
      else
        loneType
          localizer
          tipe
          expected
          ( D.fillSep
              [ "The",
                direction,
                "side",
                "of",
                "(/)",
                "must",
                "be",
                "a",
                D.dullyellow "Float" <> ",",
                "but",
                "instead",
                "I",
                "am",
                "seeing:"
              ]
          )
          []
  )

badIDiv :: L.Localizer -> D.Doc -> T.Type -> T.Type -> (D.Doc, D.Doc)
badIDiv localizer direction tipe expected =
  ( D.reflow $
      "The (//) operator is specifically for integer division:",
    if isFloat tipe
      then
        D.stack
          [ D.fillSep
              [ "The",
                direction,
                "side",
                "of",
                "(//)",
                "must",
                "be",
                "an",
                D.dullyellow "Int" <> ",",
                "but",
                "I",
                "am",
                "seeing",
                "a",
                D.dullyellow "Float" <> ".",
                "I",
                "recommend",
                "doing",
                "the",
                "conversion",
                "explicitly",
                "with",
                "one",
                "of",
                "these",
                "functions:"
              ],
            D.vcat
              [ D.green "round" <> " 3.5     == 4",
                D.green "floor" <> " 3.5     == 3",
                D.green "ceiling" <> " 3.5   == 4",
                D.green "truncate" <> " 3.5  == 3"
              ],
            D.link "Note" "Read" "implicit-casts" "to learn why Gren does not implicitly convert Ints to Floats."
          ]
      else
        loneType
          localizer
          tipe
          expected
          ( D.fillSep
              [ "The",
                direction,
                "side",
                "of",
                "(//)",
                "must",
                "be",
                "an",
                D.dullyellow "Int" <> ",",
                "but",
                "instead",
                "I",
                "am",
                "seeing:"
              ]
          )
          []
  )

-- BAD BOOLS

badBool :: L.Localizer -> D.Doc -> D.Doc -> T.Type -> T.Type -> (D.Doc, D.Doc)
badBool localizer op direction tipe expected =
  ( D.reflow $
      "I am struggling with this boolean operation:",
    loneType
      localizer
      tipe
      expected
      ( D.fillSep
          [ "Both",
            "sides",
            "of",
            "(" <> op <> ")",
            "must",
            "be",
            D.dullyellow "Bool",
            "values,",
            "but",
            "the",
            direction,
            "side",
            "is:"
          ]
      )
      []
  )

-- BAD COMPARISON

badCompLeft :: L.Localizer -> Category -> String -> String -> T.Type -> T.Type -> (D.Doc, D.Doc)
badCompLeft localizer category op direction tipe expected =
  ( D.reflow $
      "I cannot do a comparison with this value:",
    loneType
      localizer
      tipe
      expected
      (D.reflow (addCategory ("The " <> direction <> " side of (" <> op <> ") is") category))
      [ D.fillSep
          [ "But",
            "(" <> D.fromChars op <> ")",
            "only",
            "works",
            "on",
            D.dullyellow "Int" <> ",",
            D.dullyellow "Float" <> ",",
            D.dullyellow "Char" <> ",",
            "and",
            D.dullyellow "String",
            "values.",
            "It",
            "can",
            "work",
            "on",
            "arrays",
            "of",
            "comparable",
            "values",
            "as",
            "well,",
            "but",
            "it",
            "is",
            "usually",
            "better",
            "to",
            "find",
            "a",
            "different",
            "path."
          ]
      ]
  )

badCompRight :: L.Localizer -> String -> T.Type -> T.Type -> RightDocs
badCompRight localizer op tipe expected =
  EmphBoth
    ( D.reflow $
        "I need both sides of (" <> op <> ") to be the same type:",
      typeComparison
        localizer
        expected
        tipe
        ("The left side of (" <> op <> ") is:")
        "But the right side is:"
        [ D.reflow $
            "I cannot compare different types though! Which side of (" <> op <> ") is the problem?"
        ]
    )

-- BAD EQUALITY

badEquality :: L.Localizer -> String -> T.Type -> T.Type -> RightDocs
badEquality localizer op tipe expected =
  EmphBoth
    ( D.reflow $
        "I need both sides of (" <> op <> ") to be the same type:",
      typeComparison
        localizer
        expected
        tipe
        ("The left side of (" <> op <> ") is:")
        "But the right side is:"
        [ if isFloat tipe || isFloat expected
            then
              D.toSimpleNote $
                "Equality on floats is not 100% reliable due to the design of IEEE 754. I\
                \ recommend a check like (abs (x - y) < 0.0001) instead."
            else D.reflow "Different types can never be equal though! Which side is messed up?"
        ]
    )

-- INFINITE TYPES

toInfiniteReport :: Code.Source -> L.Localizer -> A.Region -> Name.Name -> T.Type -> Report.Report
toInfiniteReport source localizer region name overallType =
  Report.Report "INFINITE TYPE" region [] $
    Code.toSnippet
      source
      region
      Nothing
      ( D.reflow $
          "I am inferring a weird self-referential type for " <> Name.toChars name <> ":",
        D.stack
          [ D.reflow $
              "Here is my best effort at writing down the type. You will see ∞ for\
              \ parts of the type that repeat something already printed out infinitely.",
            D.indent 4 (D.dullyellow (T.toDoc localizer RT.None overallType)),
            D.reflowLink
              "Staring at this type is usually not so helpful, so I recommend reading the hints at"
              "infinite-type"
              "to get unstuck!"
          ]
      )
