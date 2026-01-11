module Nitpick.PatternMatchesSpec (spec) where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Data.Index qualified as Index
import Data.Map qualified as Map
import Data.Name qualified as N
import Data.NonEmptyList qualified as NE
import Data.Utf8 qualified as Utf8
import Gren.ModuleName qualified as ModuleName
import Gren.Package qualified as Pkg
import Reporting.Annotation qualified as A

import Nitpick.PatternMatches (check, Error (..))

import Test.Hspec (Spec, describe, it)

-- Create a Can.Union for Bool
boolUnion :: Can.Union
boolUnion = 
  Can.Union 
    { Can._u_vars = []
    , Can._u_alts = 
        [ Can.Ctor (N.fromChars "True") (Index.first) 0 []
        , Can.Ctor (N.fromChars "False") (Index.next (Index.first)) 0 []
        ]
    , Can._u_numAlts = 2
    , Can._u_opts = Can.Enum
    }

emptyUnions :: Map.Map N.Name Can.Union
emptyUnions = Map.empty

emptyAliases :: Map.Map N.Name Can.Alias
emptyAliases = Map.empty

emptyBinops :: Map.Map N.Name Can.Binop
emptyBinops = Map.empty

packageName :: String -> String -> Pkg.Name
packageName pkgName authorName =
    Pkg.Name
        { Pkg._author = Utf8.fromChars authorName
        , Pkg._project = Utf8.fromChars pkgName
        }

moduleNameCanonical :: String -> String -> String -> ModuleName.Canonical
moduleNameCanonical pkgName authorName modName =
    ModuleName.Canonical
        { ModuleName._package = packageName pkgName authorName
        , ModuleName._module = N.fromChars modName
        }

-- Create a Module from Decls
-- We use A.zero to give an empty Region for exports and docs, as we don't
-- care about their values
makeModule :: Can.Decls -> Can.Module
makeModule decls = 
    Can.Module
        { Can._name = moduleNameCanonical "TestPkg" "gren-devs" "TestModule"
        , Can._exports = Can.ExportEverything A.zero
        , Can._docs = Src.NoDocs A.zero
        , Can._decls = decls
        , Can._unions = emptyUnions
        , Can._aliases = emptyAliases
        , Can._binops = emptyBinops
        , Can._effects = Can.NoEffects
        }

-- In the unit test we may need to induce a failure just to appease
-- the compiler. Use this Region when doing so.
failedRegion :: A.Region
failedRegion =
    A.Region (A.Position 99 99) (A.Position 99 99)


-- Incomplete Bool Records
{-
fn r =
    when r is
        { a = False, b = True } -> 1
        { a = True, b = False } -> 2
-}
-- The AST
-- Debug.Trace trace was used to show the decls during "check",
-- and this function was entered into  "gren repl".
-- The result was used to create this AST
--
-- Since we don't care about the true column/row range of each token
-- in the source code, we use A.zero for each Region.
incompleteBoolRecordsDecls :: Can.Decls
incompleteBoolRecordsDecls =
  Can.Declare 
    (Can.Def (A.At A.zero (N.fromChars "fn"))
      [ A.At A.zero (Can.PVar (N.fromChars "r")) ]
      (A.At A.zero
        (Can.Case (A.At A.zero (Can.VarLocal (N.fromChars "r")))
          [ Can.CaseBranch 
              (A.At A.zero
                (Can.PRecord 
                  [ A.At A.zero (Can.PRFieldPattern (N.fromChars "a") (A.At A.zero (Can.PBool boolUnion False)))
                  , A.At A.zero (Can.PRFieldPattern (N.fromChars "b") (A.At A.zero (Can.PBool boolUnion True)))
                  ]
                )
              ) 
              (A.At A.zero (Can.Int 1))
          , Can.CaseBranch 
              (A.At A.zero
                (Can.PRecord 
                  [ A.At A.zero (Can.PRFieldPattern (N.fromChars "a") (A.At A.zero (Can.PBool boolUnion True)))
                  , A.At A.zero (Can.PRFieldPattern (N.fromChars "b") (A.At A.zero (Can.PBool boolUnion False)))
                  ]
                )
              ) 
              (A.At A.zero (Can.Int 2))
          ]
        )
      )
    ) 
    Can.SaveTheEnvironment


spec :: Spec
spec = do
  describe "PatternMatches tests" $ do

    it "Incomplete bool matrix fails to compile" $ do
        let
            -- result is: Either (NE.List Error) ()
            result = check (makeModule incompleteBoolRecordsDecls)
         in
            case result of
                Left neListError ->
                    let
                        err = case NE.toList neListError of
                            (x : _) -> x

                            -- Impossible, as we are using NonEmptyList
                            -- Return something we know will fail the test
                            [] -> Redundant failedRegion failedRegion 99
                    in
                        case err of
                            -- Once we get the checker to return Incomplete,
                            -- mayb we can test the additional arguments to
                            -- "Incomplete"
                            Incomplete _ _ _ -> True
                            Redundant _ _ _ -> False

                Right () ->
                    -- The check succeeds, which is NOT what we want.
                    False
