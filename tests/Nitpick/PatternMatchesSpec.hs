module Nitpick.PatternMatchesSpec (spec) where

import AST.Canonical (Pattern_)
import AST.Canonical qualified as Can
import Data.Index qualified as Index
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Name qualified as N
import Data.Utf8 qualified as Utf8
import Gren.ModuleName qualified as ModuleName
import Gren.Package qualified as Pkg
import Nitpick.PatternMatches
  ( Context (..),
    Error (..),
    Literal (..),
    Pattern (..),
    checkPatterns,
  )
import Reporting.Annotation qualified as A
import Test.Hspec (Spec, describe, it, shouldBe)

-- Create a Can.Union for Bool
boolUnion :: Can.Union
boolUnion =
  Can.Union
    { Can._u_vars = [],
      Can._u_alts =
        [ Can.Ctor (N.fromChars "True") (Index.first) 0 [],
          Can.Ctor (N.fromChars "False") (Index.next (Index.first)) 0 []
        ],
      Can._u_numAlts = 2,
      Can._u_opts = Can.Enum
    }

-- Create a Can.Union for Maybe
maybeUnion :: Can.Union
maybeUnion =
  Can.Union
    { Can._u_vars = [(N.fromChars "a")],
      Can._u_alts =
        [ Can.Ctor (N.fromChars "Just") (Index.first) 1 [Can.TVar (N.fromChars "a")],
          Can.Ctor (N.fromChars "Nothing") (Index.next (Index.first)) 0 []
        ],
      Can._u_numAlts = 2,
      Can._u_opts = Can.Normal
    }

-- Create a Pattern_ that is a PCtor for Maybe
maybePCtor :: Bool -> [Can.PatternCtorArg] -> Pattern_
maybePCtor isJust args =
  Can.PCtor
    { Can._p_home =
        ModuleName.Canonical
          (Pkg.Name (Utf8.fromChars "core") (Utf8.fromChars "gren-lang"))
          (Utf8.fromChars "Maybe"),
      Can._p_type = (N.fromChars "Maybe"),
      Can._p_union = maybeUnion,
      Can._p_name = if isJust then (N.fromChars "Just") else (N.fromChars "Nothing"),
      Can._p_index = if isJust then Index.first else (Index.next (Index.first)),
      Can._p_args = args
    }

{-
Test 1: Incomplete Bool Records

This is the original case reported by marias. It exercises
a bug in the compiler.

fn r =
    when r is
        { a = False, b = True } -> 1
        { a = True, b = False } -> 2
-}

-- These are the Can.Patterns produced by the parser and checkCases
test1InputCanPatterns :: [Can.Pattern]
test1InputCanPatterns =
  [ ( A.At
        A.zero
        ( Can.PRecord
            [ A.At A.zero (Can.PRFieldPattern (N.fromChars "a") (A.At A.zero (Can.PBool boolUnion False))),
              A.At A.zero (Can.PRFieldPattern (N.fromChars "b") (A.At A.zero (Can.PBool boolUnion True)))
            ]
        )
    ),
    ( A.At
        A.zero
        ( Can.PRecord
            [ A.At A.zero (Can.PRFieldPattern (N.fromChars "a") (A.At A.zero (Can.PBool boolUnion True))),
              A.At A.zero (Can.PRFieldPattern (N.fromChars "b") (A.At A.zero (Can.PBool boolUnion False)))
            ]
        )
    )
  ]

-- We expect isExhaustive to find these patterns as missing:
-- It finds 1 Error, which has 2 strings, one for each missing pattern
test1Expectation :: [[String]]
test1Expectation =
  [ [ "a : True, b : True",
      "a : False, b : False"
    ]
  ]

{-
Test 2: Record destructuring, as seen in core.git's Dict.gren

After an incorrect fix for test1 by gilbertr, this case was found
to need extra handling. It *is* exhaustive, but the incorrect fix
found it to be non-exhaustive.

fn r =
    when r is
        Nothing -> "Nothing"
        Just { first = { key = lKey, value = lValue }, rest } -> lKey
-}

-- These are the Can.Patterns produced by the parser and checkCases
test2InputCanPatterns :: [Can.Pattern]
test2InputCanPatterns =
  [ (A.At A.zero (maybePCtor False [])),
    ( A.At
        A.zero
        ( maybePCtor
            True
            [ Can.PatternCtorArg
                { Can._index = Index.first,
                  Can._type = Can.TVar (N.fromChars "a"),
                  Can._arg =
                    ( A.At
                        A.zero
                        ( Can.PRecord
                            [ A.At
                                A.zero
                                ( Can.PRFieldPattern
                                    (N.fromChars "first")
                                    ( A.At
                                        A.zero
                                        ( Can.PRecord
                                            [ A.At
                                                A.zero
                                                ( Can.PRFieldPattern
                                                    (N.fromChars "key")
                                                    (A.At A.zero (Can.PVar (N.fromChars "lKey")))
                                                ),
                                              A.At
                                                A.zero
                                                ( Can.PRFieldPattern
                                                    (N.fromChars "value")
                                                    (A.At A.zero (Can.PVar (N.fromChars "lValue")))
                                                )
                                            ]
                                        )
                                    )
                                ),
                              A.At
                                A.zero
                                ( Can.PRFieldPattern
                                    (N.fromChars "rest")
                                    ( A.At A.zero (Can.PVar (N.fromChars "rest"))
                                    )
                                )
                            ]
                        ) -- Can.PRecord
                    )
                }
            ] -- Can.PatternCtorArg
        )
    )
  ]

-- We expect isExhaustive to find *no* patterns to be missing:
test2Expectation :: [[String]]
test2Expectation =
  []

-- Small helper for the unit tests.
-- This takes the input [Can.Pattern] and runs
-- checkPatterns on it.
runCheckPatterns :: [Can.Pattern] -> [Error]
runCheckPatterns patterns =
  checkPatterns A.zero BadCase patterns []

-- checkPatterns returns an [Error],
-- which can have [Pattern] in it.
-- This is cumbersome to test in the unit tests.
-- We convert each Error (and thus, [Pattern]) to a [String],
-- making it a lot easier to assert on in the unit tests.
--
-- E.g., this [Error]
-- [  Incomplete A.Region Context [Pattern] ]
--
-- in test 1 has this [Pattern]
-- [ Record (fromList
--      [("a",Ctor boolUnion "True" [])
--      ,("b",Ctor boolUnion "True" [])
--      ])
-- , Record (fromList
--      [("a",Ctor boolUnion "False" [])
--      ,("b",Ctor boolUnion "False" [])
--      ])
-- ]
--
-- which we convert into:
-- [
--      [ "a: True, b: True",
--      , "b: False, b: False"
--      ]
-- ]

-- Convert a list of Error into a matrix of Strings
errorsToTestableStrings :: [Error] -> [[String]]
errorsToTestableStrings errors =
  map
    ( \err ->
        case err of
          Incomplete _ _ patterns -> (map (\pattern -> patternToTestableString pattern)) patterns
          Redundant _ _ _ -> ["redundant"]
    )
    errors

-- Given a list of Patterns, return a string representation
patternsToTestableString :: [Pattern] -> String
patternsToTestableString patterns =
  "[ " ++ (intercalate ", " (map patternToTestableString patterns)) ++ " ]"

-- Convert a single Pattern into a String
patternToTestableString :: Pattern -> String
patternToTestableString pat =
  case pat of
    Anything -> "anything"
    Literal (Chr c) -> Utf8.toChars c
    Literal (Str s) -> Utf8.toChars s
    Literal (Int n) -> show n -- convert Int to String
    Array patterns -> patternsToTestableString patterns
    Ctor _ vName patterns ->
      if null patterns
        then N.toChars vName
        else (N.toChars vName) ++ (patternsToTestableString patterns)
    Record patternMap ->
      let -- Make a new map with String values
          newValuesMap = Map.map (\vPattern -> patternToTestableString vPattern) patternMap

          -- Transform the keys into Strings
          -- (++) here is a combiner function in case of key collisions
          newMap = Map.mapKeysWith (++) (\kName -> (N.toChars kName)) newValuesMap

          -- Convert to sorted list of (key, value) pairs
          -- Maps are balanced trees in Haskell, so walking them gives us
          -- sorted already
          pairs = Map.toList newMap

          -- Map each pair to a single "k : v" string
          formattedPairs = map (\(k, v) -> k ++ " : " ++ v) pairs
       in -- Join them all into one string
          intercalate ", " formattedPairs

-- The unit tests
spec :: Spec
spec = do
  describe "PatternMatches tests" $ do
    it "Test 1 bool matrix is not exhaustive" $ do
      let errors = runCheckPatterns test1InputCanPatterns
          errorStrings = errorsToTestableStrings errors
       in errorStrings `shouldBe` test1Expectation

    it "Test 2 record destructruting is exhaustive" $ do
      let errors = runCheckPatterns test2InputCanPatterns
          errorStrings = errorsToTestableStrings errors
       in errorStrings `shouldBe` test2Expectation
