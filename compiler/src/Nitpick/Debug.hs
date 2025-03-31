module Nitpick.Debug
  ( hasDebugUses,
  )
where

import AST.Optimized qualified as Opt
import Data.Map.Utils qualified as Map
import Data.Maybe qualified as Maybe

-- HAS DEBUG USES

hasDebugUses :: Opt.LocalGraph -> Bool
hasDebugUses (Opt.LocalGraph _ graph _) =
  Map.any nodeHasDebug graph

nodeHasDebug :: Opt.Node -> Bool
nodeHasDebug node =
  case node of
    Opt.Define _ expr _ -> hasDebug expr
    Opt.DefineTailFunc _ _ expr _ -> hasDebug expr
    Opt.Ctor _ _ -> False
    Opt.Enum _ -> False
    Opt.Box -> False
    Opt.Link _ -> False
    Opt.Cycle _ vs fs _ -> any (hasDebug . snd) vs || any defHasDebug fs
    Opt.Manager _ -> False
    Opt.Kernel _ _ -> False
    Opt.PortIncoming expr _ -> hasDebug expr
    Opt.PortOutgoing expr _ -> hasDebug expr
    Opt.PortTask maybeExpr expr _ -> hasDebug expr || Maybe.maybe False hasDebug maybeExpr

hasDebug :: Opt.Expr -> Bool
hasDebug expression =
  case expression of
    Opt.Bool _ _ -> False
    Opt.Chr _ _ -> False
    Opt.Str _ _ -> False
    Opt.Int _ _ -> False
    Opt.Float _ _ -> False
    Opt.VarLocal _ _ -> False
    Opt.VarGlobal _ _ -> False
    Opt.VarEnum _ _ _ -> False
    Opt.VarBox _ _ -> False
    Opt.VarCycle _ _ _ -> False
    Opt.VarDebug _ _ _ _ -> True
    Opt.VarKernel _ _ _ -> False
    Opt.Array _ exprs -> any hasDebug exprs
    Opt.Function _ _ expr -> hasDebug expr
    Opt.Call _ e es -> hasDebug e || any hasDebug es
    Opt.TailCall _ args -> any (hasDebug . snd) args
    Opt.If conds finally -> any (\(c, e) -> hasDebug c || hasDebug e) conds || hasDebug finally
    Opt.Let def body -> defHasDebug def || hasDebug body
    Opt.Destruct _ expr -> hasDebug expr
    Opt.Case _ _ d jumps -> deciderHasDebug d || any (hasDebug . snd) jumps
    Opt.Accessor _ _ -> False
    Opt.Access r _ _ -> hasDebug r
    Opt.Update _ r fs -> hasDebug r || any hasDebug fs
    Opt.Record _ fs -> any hasDebug fs

defHasDebug :: Opt.Def -> Bool
defHasDebug def =
  case def of
    Opt.Def _ _ expr -> hasDebug expr
    Opt.TailDef _ _ _ expr -> hasDebug expr

deciderHasDebug :: Opt.Decider Opt.Choice -> Bool
deciderHasDebug decider =
  case decider of
    Opt.Leaf (Opt.Inline expr) -> hasDebug expr
    Opt.Leaf (Opt.Jump _) -> False
    Opt.Chain _ success failure -> deciderHasDebug success || deciderHasDebug failure
    Opt.FanOut _ tests fallback -> any (deciderHasDebug . snd) tests || deciderHasDebug fallback
