module Nitpick.PatternMatchesSpec (spec) where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Data.Index qualified as Index
import Data.Map qualified as Map
import Data.Name qualified as N
import Data.NonEmptyList qualified as NE
import Data.Utf8 qualified as Utf8
import Data.Word (Word16)
import Gren.ModuleName qualified as ModuleName
import Gren.Package qualified as Pkg
import Reporting.Annotation qualified as A

import Nitpick.PatternMatches (check, Error (..))

import Test.Hspec (Spec, describe, it)

-- Create a Located
at :: A.Region -> a -> A.Located a
at = A.At

-- Create a Region
region :: A.Position -> A.Position -> A.Region
region = A.Region

-- Creat a Position
pos :: Word16 -> Word16 -> A.Position
pos = A.Position

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
makeModule :: Can.Decls -> Can.Module
makeModule decls = 
    Can.Module
        { Can._name = moduleNameCanonical "TestPkg" "gren-devs" "TestModule"
        -- The region here is made up and has no pertinent meaning
        , Can._exports = Can.ExportEverything (region (pos 1 1) (pos 3 3))
        -- The region here is made up and has no pertinent meaning
        , Can._docs = Src.NoDocs (region (pos 1 1) (pos 3 3))
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
    region (pos 99 99) (pos 99 99)



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
incompleteBoolRecordsDecls :: Can.Decls
incompleteBoolRecordsDecls =
  Can.Declare 
    (Can.Def (at (region (pos 2 1) (pos 2 6)) (N.fromChars "fn")) 
      [ at (region (pos 2 7) (pos 2 8)) (Can.PVar (N.fromChars "r")) ]
      (at (region (pos 3 5) (pos 5 37)) 
        (Can.Case (at (region (pos 3 10) (pos 3 11)) (Can.VarLocal (N.fromChars "r"))) 
          [ Can.CaseBranch 
              (at (region (pos 4 9) (pos 4 32)) 
                (Can.PRecord 
                  [ at (region (pos 4 11) (pos 4 20)) (Can.PRFieldPattern (N.fromChars "a") (at (region (pos 4 15) (pos 4 20)) (Can.PBool boolUnion False)))
                  , at (region (pos 4 22) (pos 4 30)) (Can.PRFieldPattern (N.fromChars "b") (at (region (pos 4 26) (pos 4 30)) (Can.PBool boolUnion True)))
                  ]
                )
              ) 
              (at (region (pos 4 36) (pos 4 37)) (Can.Int 1))
          , Can.CaseBranch 
              (at (region (pos 5 9) (pos 5 32)) 
                (Can.PRecord 
                  [ at (region (pos 5 11) (pos 5 19)) (Can.PRFieldPattern (N.fromChars "a") (at (region (pos 5 15) (pos 5 19)) (Can.PBool boolUnion True)))
                  , at (region (pos 5 21) (pos 5 30)) (Can.PRFieldPattern (N.fromChars "b") (at (region (pos 5 25) (pos 5 30)) (Can.PBool boolUnion False)))
                  ]
                )
              ) 
              (at (region (pos 5 36) (pos 5 37)) (Can.Int 2))
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
