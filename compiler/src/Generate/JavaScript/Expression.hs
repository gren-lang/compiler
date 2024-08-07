{-# LANGUAGE OverloadedStrings #-}

module Generate.JavaScript.Expression
  ( generate,
    generateCtor,
    generateCtorImplementation,
    generateField,
    generateFunctionImplementation,
    generateCurriedFunctionRef,
    generateTailDef,
    generateTailDefImplementation,
    generateMain,
    Code (..),
    codeToExpr,
    codeToStmtList,
  )
where

import AST.Canonical qualified as Can
import AST.Optimized qualified as Opt
import Data.Index qualified as Index
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Name qualified as Name
import Data.Utf8 qualified as Utf8
import Generate.JavaScript.Builder qualified as JS
import Generate.JavaScript.Name qualified as JsName
import Generate.Mode qualified as Mode
import Gren.Compiler.Type qualified as Type
import Gren.Compiler.Type.Extract qualified as Extract
import Gren.ModuleName qualified as ModuleName
import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import Json.Encode ((==>))
import Json.Encode qualified as Encode
import Optimize.DecisionTree qualified as DT
import Reporting.Annotation qualified as A

-- EXPRESSIONS

type FnArgLookup = ModuleName.Canonical -> Name.Name -> Maybe Int

generateJsExpr :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Opt.Expr -> JS.Expr
generateJsExpr mode argLookup parentModule expression =
  codeToExpr (generate mode argLookup parentModule expression)

generate :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Opt.Expr -> Code
generate mode argLookup parentModule expression =
  case expression of
    Opt.Bool (A.Region start _) bool ->
      JsExpr $ JS.TrackedBool parentModule start bool
    Opt.Chr (A.Region start _) char ->
      JsExpr $
        case mode of
          Mode.Dev _ ->
            JS.Call toChar [JS.TrackedString parentModule start (Utf8.toBuilder char)]
          Mode.Prod _ ->
            JS.TrackedString parentModule start (Utf8.toBuilder char)
    Opt.Str (A.Region start _) string ->
      JsExpr $ JS.TrackedString parentModule start (Utf8.toBuilder string)
    Opt.Int (A.Region start _) int ->
      JsExpr $ JS.TrackedInt parentModule start int
    Opt.Float (A.Region start _) float ->
      JsExpr $ JS.TrackedFloat parentModule start (Utf8.toBuilder float)
    Opt.VarLocal (A.Region startPos _) name ->
      JsExpr $ JS.TrackedRef parentModule startPos (JsName.fromLocalHumanReadable name) (JsName.fromLocal name)
    Opt.VarGlobal (A.Region startPos _) (Opt.Global home name) ->
      JsExpr $ JS.TrackedRef parentModule startPos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobal home name)
    Opt.VarEnum (A.Region startPos _) (Opt.Global home name) index ->
      case mode of
        Mode.Dev _ ->
          JsExpr $ JS.TrackedRef parentModule startPos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobal home name)
        Mode.Prod _ ->
          JsExpr $ JS.Int (Index.toMachine index)
    Opt.VarBox (A.Region startPos _) (Opt.Global home name) ->
      JsExpr $
        case mode of
          Mode.Dev _ -> JS.TrackedRef parentModule startPos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobal home name)
          Mode.Prod _ -> JS.Ref $ JsName.fromGlobal ModuleName.basics Name.identity
    Opt.VarCycle (A.Region startPos _) home name ->
      JsExpr $ JS.Call (JS.TrackedRef parentModule startPos (JsName.fromGlobalHumanReadable home name) (JsName.fromCycle home name)) []
    Opt.VarDebug region name home unhandledValueName ->
      JsExpr $ generateDebug parentModule name home region unhandledValueName
    Opt.VarKernel (A.Region startPos _) home name ->
      JsExpr $ JS.TrackedRef parentModule startPos (JsName.fromKernel home name) (JsName.fromKernel home name)
    Opt.Array region entries ->
      let generatedEntries = map (generateJsExpr mode argLookup parentModule) entries
       in JsExpr $
            if region == A.zero
              then JS.Array generatedEntries
              else JS.TrackedArray parentModule region generatedEntries
    Opt.Function (A.Region startPos _) args body ->
      let argNames = map (\(A.At region name) -> A.At region (JsName.fromLocal name)) args
       in generateTrackedFunction parentModule startPos argNames (generate mode argLookup parentModule body)
    Opt.Call (A.Region startPos _) func args ->
      JsExpr $ generateCall mode argLookup parentModule startPos func args
    Opt.TailCall name args ->
      JsBlock $ generateTailCall mode argLookup parentModule name args
    Opt.If branches final ->
      generateIf mode argLookup parentModule branches final
    Opt.Let def body ->
      JsBlock $
        generateDef mode argLookup parentModule def : codeToStmtList (generate mode argLookup parentModule body)
    Opt.Destruct (Opt.Destructor name path) body ->
      let pathDef = JS.Var (JsName.fromLocal name) (generatePath mode path)
       in JsBlock $ pathDef : codeToStmtList (generate mode argLookup parentModule body)
    Opt.Case label root decider jumps ->
      JsBlock $ generateCase mode argLookup parentModule label root decider jumps
    Opt.Accessor _ field ->
      JsExpr $
        JS.Function
          Nothing
          [JsName.dollar]
          [ JS.Return $
              JS.Access (JS.Ref JsName.dollar) (generateField mode field)
          ]
    Opt.Access record (A.Region startPos _) field ->
      JsExpr $ JS.TrackedAccess (generateJsExpr mode argLookup parentModule record) parentModule startPos (generateField mode field)
    Opt.Update region record fields ->
      JsExpr $
        JS.Call
          (JS.Ref (JsName.fromKernel Name.utils "update"))
          [ generateJsExpr mode argLookup parentModule record,
            generateRecord mode argLookup parentModule region fields
          ]
    Opt.Record region fields ->
      JsExpr $ generateRecord mode argLookup parentModule region fields

-- CODE CHUNKS

data Code
  = JsExpr JS.Expr
  | JsBlock [JS.Stmt]

codeToExpr :: Code -> JS.Expr
codeToExpr code =
  case code of
    JsExpr expr ->
      expr
    JsBlock [JS.Return expr] ->
      expr
    JsBlock stmts ->
      JS.Call (JS.Function Nothing [] stmts) []

codeToStmtList :: Code -> [JS.Stmt]
codeToStmtList code =
  case code of
    JsExpr (JS.Call (JS.Function Nothing [] stmts) []) ->
      stmts
    JsExpr expr ->
      [JS.Return expr]
    JsBlock stmts ->
      stmts

codeToStmt :: Code -> JS.Stmt
codeToStmt code =
  case code of
    JsExpr (JS.Call (JS.Function Nothing [] stmts) []) ->
      JS.Block stmts
    JsExpr expr ->
      JS.Return expr
    JsBlock [stmt] ->
      stmt
    JsBlock stmts ->
      JS.Block stmts

-- CHARS

toChar :: JS.Expr
toChar =
  JS.Ref (JsName.fromKernel Name.utils "chr")

-- CTOR

generateCtor :: Mode.Mode -> Opt.Global -> Index.ZeroBased -> Int -> Code
generateCtor mode (Opt.Global home name) index arity =
  let argNames =
        Index.indexedMap (\i _ -> JsName.fromIndex i) [1 .. arity]

      ctorTag =
        case mode of
          Mode.Dev _ -> JS.String (Name.toBuilder name)
          Mode.Prod _ -> JS.Int (ctorToInt home name index)
   in generateFunction argNames $
        JsExpr $
          JS.Object $
            (JsName.dollar, ctorTag) : map (\n -> (n, JS.Ref n)) argNames

generateCtorImplementation :: Mode.Mode -> Opt.Global -> Index.ZeroBased -> Int -> Code
generateCtorImplementation mode (Opt.Global home name) index arity =
  let argNames =
        Index.indexedMap (\i _ -> JsName.fromIndex i) [1 .. arity]

      ctorTag =
        case mode of
          Mode.Dev _ -> JS.String (Name.toBuilder name)
          Mode.Prod _ -> JS.Int (ctorToInt home name index)
   in JsExpr $
        JS.Function Nothing argNames $
          codeToStmtList $
            JsExpr $
              JS.Object $
                (JsName.dollar, ctorTag) : map (\n -> (n, JS.Ref n)) argNames

ctorToInt :: ModuleName.Canonical -> Name.Name -> Index.ZeroBased -> Int
ctorToInt home name index =
  if home == ModuleName.dict && name == "RBNode_gren_builtin" || name == "RBEmpty_gren_builtin"
    then 0 - Index.toHuman index
    else Index.toMachine index

-- RECORDS

generateRecord :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> A.Region -> Map.Map (A.Located Name.Name) Opt.Expr -> JS.Expr
generateRecord mode argLookup parentModule region fields =
  let toPair (A.At fieldRegion field, value) =
        (A.At fieldRegion $ generateField mode field, generateJsExpr mode argLookup parentModule value)
   in JS.TrackedObject parentModule region (map toPair (Map.toList fields))

generateField :: Mode.Mode -> Name.Name -> JsName.Name
generateField mode name =
  case mode of
    Mode.Dev _ ->
      JsName.fromLocal name
    Mode.Prod fields ->
      fields ! name

-- DEBUG

generateDebug :: ModuleName.Canonical -> Name.Name -> ModuleName.Canonical -> A.Region -> Maybe Name.Name -> JS.Expr
generateDebug parentMod name (ModuleName.Canonical _ home) region@(A.Region startPos _) unhandledValueName =
  let trackedRef = JS.TrackedRef parentMod startPos (JsName.fromGlobalHumanReadable ModuleName.debug name) (JsName.fromGlobal ModuleName.debug name)
   in if name /= "todo"
        then trackedRef
        else case unhandledValueName of
          Nothing ->
            JS.Call trackedRef $
              [ JS.String (Name.toBuilder home),
                regionToJsExpr region
              ]
          Just valueName ->
            JS.Call (JS.Ref (JsName.fromKernel Name.debug "todoCase")) $
              [ JS.String (Name.toBuilder home),
                regionToJsExpr region,
                JS.Ref (JsName.fromLocal valueName)
              ]

regionToJsExpr :: A.Region -> JS.Expr
regionToJsExpr (A.Region start end) =
  JS.Object
    [ (JsName.fromLocal "start", positionToJsExpr start),
      (JsName.fromLocal "end", positionToJsExpr end)
    ]

positionToJsExpr :: A.Position -> JS.Expr
positionToJsExpr (A.Position line column) =
  JS.Object
    [ (JsName.fromLocal "line", JS.Int (fromIntegral line)),
      (JsName.fromLocal "column", JS.Int (fromIntegral column))
    ]

-- FUNCTION

generateFunction :: [JsName.Name] -> Code -> Code
generateFunction args body =
  case IntMap.lookup (length args) funcHelpers of
    Just helper ->
      JsExpr $
        JS.Call
          helper
          [ JS.Function Nothing args $
              codeToStmtList body
          ]
    Nothing ->
      let addArg arg code =
            JsExpr $
              JS.Function Nothing [arg] $
                codeToStmtList code
       in foldr addArg body args

generateCurriedFunctionRef :: [JsName.Name] -> JsName.Name -> Code
generateCurriedFunctionRef args ref =
  case IntMap.lookup (length args) funcHelpers of
    Just helper ->
      JsExpr $
        JS.Call
          helper
          [ JS.Ref ref
          ]
    Nothing ->
      let addArg arg code =
            JsExpr $
              JS.Function Nothing [arg] $
                codeToStmtList code
       in foldr addArg (JsExpr $ JS.Call (JS.Ref ref) (map JS.Ref args)) args

generateTrackedFunction :: ModuleName.Canonical -> A.Position -> [A.Located JsName.Name] -> Code -> Code
generateTrackedFunction parentModule pos args body =
  case IntMap.lookup (length args) funcHelpers of
    Just helper ->
      JsExpr $
        JS.Call
          helper
          [ JS.TrackedFunction parentModule pos args $
              codeToStmtList body
          ]
    Nothing ->
      case args of
        [_] ->
          JsExpr $
            JS.TrackedFunction parentModule pos args $
              codeToStmtList body
        _ ->
          let addArg arg code =
                JsExpr $
                  JS.Function Nothing [arg] $
                    codeToStmtList code
           in foldr addArg body (map A.toValue args)

funcHelpers :: IntMap.IntMap JS.Expr
funcHelpers =
  IntMap.fromList $
    map (\n -> (n, JS.Ref (JsName.makeF n))) [2 .. 9]

-- CALLS

generateCall :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> A.Position -> Opt.Expr -> [Opt.Expr] -> JS.Expr
generateCall mode argLookup parentModule pos func args =
  case func of
    Opt.VarGlobal _ global@(Opt.Global (ModuleName.Canonical pkg _) _)
      | pkg == Pkg.core ->
          generateCoreCall mode argLookup parentModule pos global args
    Opt.VarGlobal _ (Opt.Global home name) ->
      generateGlobalCall argLookup parentModule pos home name (map (generateJsExpr mode argLookup parentModule) args)
    Opt.VarBox _ (Opt.Global home name) ->
      case mode of
        Mode.Dev _ ->
          generateGlobalCall argLookup parentModule pos home name (map (generateJsExpr mode argLookup parentModule) args)
        Mode.Prod _ ->
          case args of
            [arg] ->
              generateJsExpr mode argLookup parentModule arg
            _ ->
              generateGlobalCall argLookup parentModule pos home name (map (generateJsExpr mode argLookup parentModule) args)
    _ ->
      generateCallHelp mode argLookup parentModule pos func args

generateCallHelp :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> A.Position -> Opt.Expr -> [Opt.Expr] -> JS.Expr
generateCallHelp mode argLookup parentModule pos func args =
  generateNormalCall
    parentModule
    pos
    (generateJsExpr mode argLookup parentModule func)
    (map (generateJsExpr mode argLookup parentModule) args)

generateGlobalCall :: FnArgLookup -> ModuleName.Canonical -> A.Position -> ModuleName.Canonical -> Name.Name -> [JS.Expr] -> JS.Expr
generateGlobalCall argLookup parentModule pos@(A.Position line col) home name args =
  case argLookup home name of
    Just n
      | n > 1 && n == (length args) ->
          JS.Call (JS.TrackedRef parentModule pos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobalDirectFn home name)) args
    _ ->
      let ref =
            if line == 0 && col == 0
              then JS.Ref (JsName.fromGlobal home name)
              else JS.TrackedRef parentModule pos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobal home name)
       in generateNormalCall parentModule pos ref args

generateNormalCall :: ModuleName.Canonical -> A.Position -> JS.Expr -> [JS.Expr] -> JS.Expr
generateNormalCall parentModule pos func args =
  case IntMap.lookup (length args) callHelpers of
    Just helper ->
      JS.TrackedNormalCall parentModule pos helper func args
    Nothing ->
      List.foldl' (\f a -> JS.Call f [a]) func args

callHelpers :: IntMap.IntMap JS.Expr
callHelpers =
  IntMap.fromList $
    map (\n -> (n, JS.Ref (JsName.makeA n))) [2 .. 9]

-- CORE CALLS

generateCoreCall :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> A.Position -> Opt.Global -> [Opt.Expr] -> JS.Expr
generateCoreCall mode argLookup parentModule pos (Opt.Global home@(ModuleName.Canonical _ moduleName) name) args =
  if moduleName == Name.basics
    then generateBasicsCall mode argLookup parentModule pos home name args
    else
      if moduleName == Name.bitwise
        then generateBitwiseCall argLookup parentModule pos home name (map (generateJsExpr mode argLookup parentModule) args)
        else
          if moduleName == Name.math
            then generateMathCall argLookup parentModule pos home name (map (generateJsExpr mode argLookup parentModule) args)
            else generateGlobalCall argLookup parentModule pos home name (map (generateJsExpr mode argLookup parentModule) args)

generateBitwiseCall :: FnArgLookup -> ModuleName.Canonical -> A.Position -> ModuleName.Canonical -> Name.Name -> [JS.Expr] -> JS.Expr
generateBitwiseCall argLookup parentModule pos home name args =
  case args of
    [arg] ->
      case name of
        "complement" -> JS.Prefix JS.PrefixComplement arg
        _ -> generateGlobalCall argLookup parentModule pos home name args
    [left, right] ->
      case name of
        "and" -> JS.Infix JS.OpBitwiseAnd left right
        "or" -> JS.Infix JS.OpBitwiseOr left right
        "xor" -> JS.Infix JS.OpBitwiseXor left right
        "shiftLeftBy" -> JS.Infix JS.OpLShift right left
        "shiftRightBy" -> JS.Infix JS.OpSpRShift right left
        "shiftRightZfBy" -> JS.Infix JS.OpZfRShift right left
        _ -> generateGlobalCall argLookup parentModule pos home name args
    _ ->
      generateGlobalCall argLookup parentModule pos home name args

generateBasicsCall :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> A.Position -> ModuleName.Canonical -> Name.Name -> [Opt.Expr] -> JS.Expr
generateBasicsCall mode argLookup parentModule pos home name args =
  case args of
    [grenArg] ->
      let arg = generateJsExpr mode argLookup parentModule grenArg
       in case name of
            "not" -> JS.Prefix JS.PrefixNot arg
            "negate" -> JS.Prefix JS.PrefixNegate arg
            "toFloat" -> arg
            _ -> generateGlobalCall argLookup parentModule pos home name [arg]
    [grenLeft, grenRight] ->
      case name of
        "append" -> append mode argLookup parentModule grenLeft grenRight
        "apL" -> generateJsExpr mode argLookup parentModule $ apply grenLeft grenRight
        "apR" -> generateJsExpr mode argLookup parentModule $ apply grenRight grenLeft
        _ ->
          let left = generateJsExpr mode argLookup parentModule grenLeft
              right = generateJsExpr mode argLookup parentModule grenRight
           in case name of
                "add" -> JS.Infix JS.OpAdd left right
                "sub" -> JS.Infix JS.OpSub left right
                "mul" -> JS.Infix JS.OpMul left right
                "fdiv" -> JS.Infix JS.OpDiv left right
                "idiv" -> JS.Infix JS.OpBitwiseOr (JS.Infix JS.OpDiv left right) (JS.Int 0)
                "eq" -> equal left right
                "neq" -> notEqual left right
                "lt" -> cmp JS.OpLt JS.OpLt 0 left right
                "gt" -> cmp JS.OpGt JS.OpGt 0 left right
                "le" -> cmp JS.OpLe JS.OpLt 1 left right
                "ge" -> cmp JS.OpGe JS.OpGt (-1) left right
                "or" -> JS.Infix JS.OpOr left right
                "and" -> JS.Infix JS.OpAnd left right
                "xor" -> JS.Infix JS.OpNe left right
                _ -> generateGlobalCall argLookup parentModule pos home name [left, right]
    _ ->
      generateGlobalCall argLookup parentModule pos home name (map (generateJsExpr mode argLookup parentModule) args)

generateMathCall :: FnArgLookup -> ModuleName.Canonical -> A.Position -> ModuleName.Canonical -> Name.Name -> [JS.Expr] -> JS.Expr
generateMathCall argLookup parentModule pos home name args =
  case args of
    [left, right] ->
      case name of
        "remainderBy" -> JS.Infix JS.OpMod right left
        _ -> generateGlobalCall argLookup parentModule pos home name [left, right]
    _ ->
      generateGlobalCall argLookup parentModule pos home name args

equal :: JS.Expr -> JS.Expr -> JS.Expr
equal left right =
  if isLiteral left || isLiteral right
    then strictEq left right
    else JS.Call (JS.Ref (JsName.fromKernel Name.utils "eq")) [left, right]

notEqual :: JS.Expr -> JS.Expr -> JS.Expr
notEqual left right =
  if isLiteral left || isLiteral right
    then strictNEq left right
    else
      JS.Prefix JS.PrefixNot $
        JS.Call (JS.Ref (JsName.fromKernel Name.utils "eq")) [left, right]

cmp :: JS.InfixOp -> JS.InfixOp -> Int -> JS.Expr -> JS.Expr -> JS.Expr
cmp idealOp backupOp backupInt left right =
  if isLiteral left || isLiteral right
    then JS.Infix idealOp left right
    else
      JS.Infix
        backupOp
        (JS.Call (JS.Ref (JsName.fromKernel Name.utils "cmp")) [left, right])
        (JS.Int backupInt)

isLiteral :: JS.Expr -> Bool
isLiteral expr =
  case expr of
    JS.String _ -> True
    JS.TrackedString _ _ _ -> True
    JS.Float _ -> True
    JS.TrackedFloat _ _ _ -> True
    JS.Int _ -> True
    JS.TrackedInt _ _ _ -> True
    JS.Bool _ -> True
    JS.TrackedBool _ _ _ -> True
    _ ->
      False

apply :: Opt.Expr -> Opt.Expr -> Opt.Expr
apply func value =
  case func of
    Opt.Accessor region field ->
      Opt.Access value region field
    Opt.Call region f args ->
      Opt.Call region f (args ++ [value])
    _ ->
      Opt.Call (Maybe.fromMaybe A.zero (exprRegion func)) func [value]

exprRegion :: Opt.Expr -> Maybe A.Region
exprRegion expr =
  case expr of
    Opt.Bool region _ -> Just region
    Opt.Chr region _ -> Just region
    Opt.Str region _ -> Just region
    Opt.Int region _ -> Just region
    Opt.Float region _ -> Just region
    Opt.VarLocal region _ -> Just region
    Opt.VarGlobal region _ -> Just region
    Opt.VarEnum region _ _ -> Just region
    Opt.VarBox region _ -> Just region
    Opt.VarCycle region _ _ -> Just region
    Opt.VarDebug region _ _ _ -> Just region
    Opt.VarKernel region _ _ -> Just region
    Opt.Array region _ -> Just region
    Opt.Function _ _ _ -> Nothing
    Opt.Call region _ _ -> Just region
    Opt.TailCall _ _ -> Nothing
    Opt.If _ _ -> Nothing
    Opt.Let _ _ -> Nothing
    Opt.Destruct _ _ -> Nothing
    Opt.Case _ _ _ _ -> Nothing
    Opt.Accessor region _ -> Just region
    Opt.Access _ region _ -> Just region
    Opt.Update region _ _ -> Just region
    Opt.Record region _ -> Just region

append :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Opt.Expr -> Opt.Expr -> JS.Expr
append mode argLookup parentModule left right =
  let seqs = generateJsExpr mode argLookup parentModule left : toSeqs mode argLookup parentModule right
   in if any isStringLiteral seqs
        then foldr1 (JS.Infix JS.OpAdd) seqs
        else foldr1 jsAppend seqs

jsAppend :: JS.Expr -> JS.Expr -> JS.Expr
jsAppend a b =
  JS.Call (JS.Ref (JsName.fromKernel Name.utils "ap")) [a, b]

toSeqs :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Opt.Expr -> [JS.Expr]
toSeqs mode argLookup parentModule expr =
  case expr of
    Opt.Call _ (Opt.VarGlobal _ (Opt.Global home "append")) [left, right]
      | home == ModuleName.basics ->
          generateJsExpr mode argLookup parentModule left : toSeqs mode argLookup parentModule right
    _ ->
      [generateJsExpr mode argLookup parentModule expr]

isStringLiteral :: JS.Expr -> Bool
isStringLiteral expr =
  case expr of
    JS.String _ ->
      True
    JS.TrackedString _ _ _ ->
      True
    _ ->
      False

-- SIMPLIFY INFIX OPERATORS

strictEq :: JS.Expr -> JS.Expr -> JS.Expr
strictEq left right =
  case left of
    JS.Int 0 ->
      JS.Prefix JS.PrefixNot right
    JS.Bool bool ->
      if bool then right else JS.Prefix JS.PrefixNot right
    _ ->
      case right of
        JS.Int 0 ->
          JS.Prefix JS.PrefixNot left
        JS.Bool bool ->
          if bool then left else JS.Prefix JS.PrefixNot left
        _ ->
          JS.Infix JS.OpEq left right

strictNEq :: JS.Expr -> JS.Expr -> JS.Expr
strictNEq left right =
  case left of
    JS.Int 0 ->
      JS.Prefix JS.PrefixNot (JS.Prefix JS.PrefixNot right)
    JS.Bool bool ->
      if bool then JS.Prefix JS.PrefixNot right else right
    _ ->
      case right of
        JS.Int 0 ->
          JS.Prefix JS.PrefixNot (JS.Prefix JS.PrefixNot left)
        JS.Bool bool ->
          if bool then JS.Prefix JS.PrefixNot left else left
        _ ->
          JS.Infix JS.OpNe left right

-- TAIL CALL

generateTailCall :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Name.Name -> [(Name.Name, Opt.Expr)] -> [JS.Stmt]
generateTailCall mode argLookup parentModule name args =
  let toTempVars (argName, arg) =
        (JsName.makeTemp argName, generateJsExpr mode argLookup parentModule arg)

      toRealVars (argName, _) =
        JS.ExprStmt $
          JS.Assign (JS.LRef (JsName.fromLocal argName)) (JS.Ref (JsName.makeTemp argName))
   in JS.Vars (map toTempVars args)
        : map toRealVars args
        ++ [JS.Continue (Just (JsName.fromLocal name))]

-- DEFINITIONS

generateDef :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Opt.Def -> JS.Stmt
generateDef mode argLookup parentModule def =
  case def of
    Opt.Def (A.Region start _) name body ->
      JS.TrackedVar parentModule start (JsName.fromLocal name) (JsName.fromLocal name) $
        generateJsExpr mode argLookup parentModule body
    Opt.TailDef (A.Region start _) name argNames body ->
      JS.TrackedVar parentModule start (JsName.fromLocal name) (JsName.fromLocal name) $
        codeToExpr (generateTailDef mode argLookup parentModule name argNames body)

generateFunctionImplementation :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> A.Position -> [A.Located Name.Name] -> Opt.Expr -> Code
generateFunctionImplementation mode argLookup parentModule funcPos argNames body =
  JsExpr $
    JS.TrackedFunction parentModule funcPos (map (\(A.At region argName) -> A.At region (JsName.fromLocal argName)) argNames) $
      codeToStmtList $
        generate mode argLookup parentModule body

generateTailDef :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Name.Name -> [A.Located Name.Name] -> Opt.Expr -> Code
generateTailDef mode argLookup parentModule name argNames body =
  generateTrackedFunction parentModule A.zeroPosition (map (\(A.At region argName) -> A.At region (JsName.fromLocal argName)) argNames) $
    JsBlock
      [ JS.Labelled (JsName.fromLocal name) $
          JS.While (JS.Bool True) $
            codeToStmt $
              generate mode argLookup parentModule body
      ]

generateTailDefImplementation :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Name.Name -> [A.Located Name.Name] -> Opt.Expr -> Code
generateTailDefImplementation mode argLookup parentModule name argNames body =
  JsExpr $
    JS.TrackedFunction parentModule A.zeroPosition (map (\(A.At region argName) -> A.At region (JsName.fromLocal argName)) argNames) $
      codeToStmtList $
        JsBlock
          [ JS.Labelled (JsName.fromLocal name) $
              JS.While (JS.Bool True) $
                codeToStmt $
                  generate mode argLookup parentModule body
          ]

-- PATHS

generatePath :: Mode.Mode -> Opt.Path -> JS.Expr
generatePath mode path =
  case path of
    Opt.Index index subPath ->
      JS.Access (generatePath mode subPath) (JsName.fromIndex index)
    Opt.ArrayIndex index subPath ->
      JS.Index (generatePath mode subPath) (JS.Int (Index.toMachine index))
    Opt.Root name ->
      JS.Ref (JsName.fromLocal name)
    Opt.Field field subPath ->
      JS.Access (generatePath mode subPath) (generateField mode field)
    Opt.Unbox subPath ->
      case mode of
        Mode.Dev _ ->
          JS.Access (generatePath mode subPath) (JsName.fromIndex Index.first)
        Mode.Prod _ ->
          generatePath mode subPath

-- GENERATE IFS

generateIf :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> Code
generateIf mode argLookup parentModule givenBranches givenFinal =
  let (branches, final) =
        crushIfs givenBranches givenFinal

      convertBranch (condition, expr) =
        ( generateJsExpr mode argLookup parentModule condition,
          generate mode argLookup parentModule expr
        )

      branchExprs = map convertBranch branches
      finalCode = generate mode argLookup parentModule final
   in if isBlock finalCode || any (isBlock . snd) branchExprs
        then JsBlock [foldr addStmtIf (codeToStmt finalCode) branchExprs]
        else JsExpr $ foldr addExprIf (codeToExpr finalCode) branchExprs

addExprIf :: (JS.Expr, Code) -> JS.Expr -> JS.Expr
addExprIf (condition, branch) final =
  JS.If condition (codeToExpr branch) final

addStmtIf :: (JS.Expr, Code) -> JS.Stmt -> JS.Stmt
addStmtIf (condition, branch) final =
  JS.IfStmt condition (codeToStmt branch) final

isBlock :: Code -> Bool
isBlock code =
  case code of
    JsBlock _ -> True
    JsExpr _ -> False

crushIfs :: [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> ([(Opt.Expr, Opt.Expr)], Opt.Expr)
crushIfs branches final =
  crushIfsHelp [] branches final

crushIfsHelp ::
  [(Opt.Expr, Opt.Expr)] ->
  [(Opt.Expr, Opt.Expr)] ->
  Opt.Expr ->
  ([(Opt.Expr, Opt.Expr)], Opt.Expr)
crushIfsHelp visitedBranches unvisitedBranches final =
  case unvisitedBranches of
    [] ->
      case final of
        Opt.If subBranches subFinal ->
          crushIfsHelp visitedBranches subBranches subFinal
        _ ->
          (reverse visitedBranches, final)
    visiting : unvisited ->
      crushIfsHelp (visiting : visitedBranches) unvisited final

-- CASE EXPRESSIONS

generateCase :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> [(Int, Opt.Expr)] -> [JS.Stmt]
generateCase mode argLookup parentModule label root decider jumps =
  foldr (goto mode argLookup parentModule label) (generateDecider mode argLookup parentModule label root decider) jumps

goto :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Name.Name -> (Int, Opt.Expr) -> [JS.Stmt] -> [JS.Stmt]
goto mode argLookup parentModule label (index, branch) stmts =
  let labeledDeciderStmt =
        JS.Labelled
          (JsName.makeLabel label index)
          (JS.While (JS.Bool True) (JS.Block stmts))
   in labeledDeciderStmt : codeToStmtList (generate mode argLookup parentModule branch)

generateDecider :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> [JS.Stmt]
generateDecider mode argLookup parentModule label root decisionTree =
  case decisionTree of
    Opt.Leaf (Opt.Inline branch) ->
      codeToStmtList (generate mode argLookup parentModule branch)
    Opt.Leaf (Opt.Jump index) ->
      [JS.Break (Just (JsName.makeLabel label index))]
    Opt.Chain testChain success failure ->
      [ JS.IfStmt
          (List.foldl1' (JS.Infix JS.OpAnd) (map (generateIfTest mode root) testChain))
          (JS.Block $ generateDecider mode argLookup parentModule label root success)
          (JS.Block $ generateDecider mode argLookup parentModule label root failure)
      ]
    Opt.FanOut path edges fallback ->
      [ JS.Switch
          (generateCaseTest mode root path (fst (head edges)))
          ( foldr
              (\edge cases -> generateCaseBranch mode argLookup parentModule label root edge : cases)
              [JS.Default (generateDecider mode argLookup parentModule label root fallback)]
              edges
          )
      ]

generateIfTest :: Mode.Mode -> Name.Name -> (DT.Path, DT.Test) -> JS.Expr
generateIfTest mode root (path, test) =
  let value = pathToJsExpr mode root path
   in case test of
        DT.IsCtor home name index _ opts ->
          let tag =
                case mode of
                  Mode.Dev _ -> JS.Access value JsName.dollar
                  Mode.Prod _ ->
                    case opts of
                      Can.Normal -> JS.Access value JsName.dollar
                      Can.Enum -> value
                      Can.Unbox -> value
           in strictEq tag $
                case mode of
                  Mode.Dev _ -> JS.String (Name.toBuilder name)
                  Mode.Prod _ -> JS.Int (ctorToInt home name index)
        DT.IsBool True ->
          value
        DT.IsBool False ->
          JS.Prefix JS.PrefixNot value
        DT.IsInt int ->
          strictEq value (JS.Int int)
        DT.IsChr char ->
          strictEq (JS.String (Utf8.toBuilder char)) $
            case mode of
              Mode.Dev _ -> JS.Call (JS.Access value (JsName.fromLocal "valueOf")) []
              Mode.Prod _ -> value
        DT.IsStr string ->
          strictEq value (JS.String (Utf8.toBuilder string))
        DT.IsArray len ->
          JS.Infix
            JS.OpEq
            (JS.Access value (JsName.fromLocal "length"))
            (JS.Int len)
        DT.IsRecord ->
          error "COMPILER BUG - there should never be tests on a record"

generateCaseBranch :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Name.Name -> Name.Name -> (DT.Test, Opt.Decider Opt.Choice) -> JS.Case
generateCaseBranch mode argLookup parentModule label root (test, subTree) =
  JS.Case
    (generateCaseValue mode test)
    (generateDecider mode argLookup parentModule label root subTree)

generateCaseValue :: Mode.Mode -> DT.Test -> JS.Expr
generateCaseValue mode test =
  case test of
    DT.IsCtor home name index _ _ ->
      case mode of
        Mode.Dev _ -> JS.String (Name.toBuilder name)
        Mode.Prod _ -> JS.Int (ctorToInt home name index)
    DT.IsInt int ->
      JS.Int int
    DT.IsChr char ->
      JS.String (Utf8.toBuilder char)
    DT.IsStr string ->
      JS.String (Utf8.toBuilder string)
    DT.IsArray len ->
      JS.Int len
    DT.IsBool _ ->
      error "COMPILER BUG - there should never be three tests on a boolean"
    DT.IsRecord ->
      error "COMPILER BUG - there should never be three tests on a record"

generateCaseTest :: Mode.Mode -> Name.Name -> DT.Path -> DT.Test -> JS.Expr
generateCaseTest mode root path exampleTest =
  let value = pathToJsExpr mode root path
   in case exampleTest of
        DT.IsCtor home name _ _ opts ->
          if name == Name.bool && home == ModuleName.basics
            then value
            else case mode of
              Mode.Dev _ ->
                JS.Access value JsName.dollar
              Mode.Prod _ ->
                case opts of
                  Can.Normal ->
                    JS.Access value JsName.dollar
                  Can.Enum ->
                    value
                  Can.Unbox ->
                    value
        DT.IsInt _ ->
          value
        DT.IsStr _ ->
          value
        DT.IsChr _ ->
          case mode of
            Mode.Dev _ ->
              JS.Call (JS.Access value (JsName.fromLocal "valueOf")) []
            Mode.Prod _ ->
              value
        DT.IsArray _ ->
          JS.Access value (JsName.fromLocal "length")
        DT.IsBool _ ->
          error "COMPILER BUG - there should never be three tests on a list"
        DT.IsRecord ->
          error "COMPILER BUG - there should never be three tests on a record"

-- PATTERN PATHS

pathToJsExpr :: Mode.Mode -> Name.Name -> DT.Path -> JS.Expr
pathToJsExpr mode root path =
  case path of
    DT.Index index subPath ->
      JS.Access (pathToJsExpr mode root subPath) (JsName.fromIndex index)
    DT.ArrayIndex index subPath ->
      JS.Index (pathToJsExpr mode root subPath) (JS.Int (Index.toMachine index))
    DT.RecordField fieldName subPath ->
      JS.Access (pathToJsExpr mode root subPath) (generateField mode fieldName)
    DT.Unbox subPath ->
      case mode of
        Mode.Dev _ ->
          JS.Access (pathToJsExpr mode root subPath) (JsName.fromIndex Index.first)
        Mode.Prod _ ->
          pathToJsExpr mode root subPath
    DT.Empty ->
      JS.Ref (JsName.fromLocal root)

-- GENERATE MAIN

generateMain :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Opt.Main -> JS.Expr
generateMain mode argLookup home main =
  case main of
    Opt.StaticString ->
      JS.Ref (JsName.fromKernel Name.node "log")
        # JS.Ref (JsName.fromGlobal home "main")
    Opt.StaticVDom ->
      JS.Ref (JsName.fromKernel Name.virtualDom "init")
        # JS.Ref (JsName.fromGlobal home "main")
        # JS.Int 0
        # JS.Int 0
    Opt.Dynamic msgType decoder ->
      JS.Ref (JsName.fromGlobal home "main")
        # generateJsExpr mode argLookup home decoder
        # toDebugMetadata mode msgType

(#) :: JS.Expr -> JS.Expr -> JS.Expr
(#) func arg =
  JS.Call func [arg]

toDebugMetadata :: Mode.Mode -> Can.Type -> JS.Expr
toDebugMetadata mode msgType =
  case mode of
    Mode.Prod _ ->
      JS.Int 0
    Mode.Dev Nothing ->
      JS.Int 0
    Mode.Dev (Just interfaces) ->
      JS.Json $
        Encode.object $
          [ "versions" ==> Encode.object ["gren" ==> V.encode V.compiler],
            "types" ==> Type.encodeMetadata (Extract.fromMsg interfaces msgType)
          ]
