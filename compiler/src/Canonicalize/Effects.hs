{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Canonicalize.Effects
  ( canonicalize,
    checkPayload,
  )
where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import AST.Utils.Type qualified as Type
import Canonicalize.Environment qualified as Env
import Canonicalize.Type qualified as Type
import Data.Foldable qualified as F
import Data.Map qualified as Map
import Data.Name qualified as Name
import Gren.ModuleName qualified as ModuleName
import Reporting.Annotation qualified as A
import Reporting.Error.Canonicalize qualified as Error
import Reporting.Result qualified as Result

-- RESULT

type Result i w a =
  Result.Result i w Error.Error a

-- CANONICALIZE

canonicalize ::
  Env.Env ->
  [A.Located Src.Value] ->
  Map.Map Name.Name union ->
  Src.Effects ->
  Result i w Can.Effects
canonicalize env values unions effects =
  case effects of
    Src.NoEffects ->
      Result.ok Can.NoEffects
    Src.Ports ports _ ->
      do
        pairs <- traverse (canonicalizePort env) (fmap snd ports)
        return $ Can.Ports (Map.fromList pairs)
    Src.Manager region manager _ ->
      let dict = Map.fromList (map toNameRegion values)
       in Can.Manager
            <$> verifyManager region dict "init"
            <*> verifyManager region dict "onEffects"
            <*> verifyManager region dict "onSelfMsg"
            <*> case manager of
              Src.Cmd cmdType _ ->
                Can.Cmd
                  <$> verifyEffectType cmdType unions
                  <* verifyManager region dict "cmdMap"
              Src.Sub subType _ ->
                Can.Sub
                  <$> verifyEffectType subType unions
                  <* verifyManager region dict "subMap"
              Src.Fx cmdType subType _ ->
                Can.Fx
                  <$> verifyEffectType cmdType unions
                  <*> verifyEffectType subType unions
                  <* verifyManager region dict "cmdMap"
                  <* verifyManager region dict "subMap"

-- CANONICALIZE PORT

canonicalizePort :: Env.Env -> Src.Port -> Result i w (Name.Name, Can.Port)
canonicalizePort env (Src.Port (A.At region portName) tipe) =
  do
    (Can.Forall freeVars ctipe) <- Type.toAnnotation env tipe
    case reverse (Type.delambda (Type.deepDealias ctipe)) of
      Can.TType home name [msg] : revArgs
        | home == ModuleName.cmd && name == Name.cmd ->
            case revArgs of
              [] ->
                Result.throw (Error.PortTypeInvalid region portName Error.CmdNoArg)
              [outgoingType] ->
                case msg of
                  Can.TVar _ ->
                    case checkPayload outgoingType of
                      Right () ->
                        Result.ok (portName, Can.Outgoing freeVars outgoingType ctipe)
                      Left (badType, err) ->
                        Result.throw (Error.PortPayloadInvalid region portName badType err)
                  _ ->
                    Result.throw (Error.PortTypeInvalid region portName Error.CmdBadMsg)
              _ ->
                Result.throw (Error.PortTypeInvalid region portName (Error.CmdExtraArgs (length revArgs)))
        | home == ModuleName.sub && name == Name.sub ->
            case revArgs of
              [Can.TLambda incomingType (Can.TVar msg1)] ->
                case msg of
                  Can.TVar msg2 | msg1 == msg2 ->
                    case checkPayload incomingType of
                      Right () ->
                        Result.ok (portName, Can.Incoming freeVars incomingType ctipe)
                      Left (badType, err) ->
                        Result.throw (Error.PortPayloadInvalid region portName badType err)
                  _ ->
                    Result.throw (Error.PortTypeInvalid region portName Error.SubBad)
              _ ->
                Result.throw (Error.PortTypeInvalid region portName Error.SubBad)
      [Can.TType home name taskArgs]
        | home == ModuleName.platform && name == Name.task ->
            case taskArgs of
              [] ->
                Result.throw (Error.PortTypeInvalid region portName Error.TaskNoArg)
              [_] ->
                Result.throw (Error.PortTypeInvalid region portName Error.TaskOneArg)
              [errorType, incomingType] ->
                case (checkTaskError errorType, checkPayload incomingType) of
                  (True, Right ()) ->
                    Result.ok (portName, Can.Task freeVars incomingType ctipe)
                  (False, _) ->
                    Result.throw (Error.PortTypeInvalid region portName Error.TaskBadError)
                  (_, Left (badType, err)) ->
                    Result.throw (Error.PortPayloadInvalid region portName badType err)
              _ ->
                Result.throw (Error.PortTypeInvalid region portName (Error.TaskExtraArgs (length taskArgs)))
      _ ->
        Result.throw (Error.PortTypeInvalid region portName Error.NotCmdOrSub)

-- VERIFY MANAGER

verifyEffectType :: A.Located Name.Name -> Map.Map Name.Name a -> Result i w Name.Name
verifyEffectType (A.At region name) unions =
  if Map.member name unions
    then Result.ok name
    else Result.throw (Error.EffectNotFound region name)

toNameRegion :: A.Located Src.Value -> (Name.Name, A.Region)
toNameRegion (A.At _ (Src.Value (A.At region name) _ _ _ _)) =
  (name, region)

verifyManager :: A.Region -> Map.Map Name.Name A.Region -> Name.Name -> Result i w A.Region
verifyManager tagRegion values name =
  case Map.lookup name values of
    Just region ->
      Result.ok region
    Nothing ->
      Result.throw (Error.EffectFunctionNotFound tagRegion name)

-- CHECK PAYLOAD TYPES

checkTaskError :: Can.Type -> Bool
checkTaskError tipe =
  case tipe of
    Can.TType home name []
      | home == ModuleName.jsonEncode && name == Name.value ->
          True
    _ -> False

checkPayload :: Can.Type -> Either (Can.Type, Error.InvalidPayload) ()
checkPayload tipe =
  case tipe of
    Can.TAlias _ _ args aliasedType ->
      checkPayload (Type.dealias args aliasedType)
    Can.TType home name args ->
      case args of
        []
          | isJson home name -> Right ()
          | isString home name -> Right ()
          | isUnit home name -> Right ()
          | isIntFloatBool home name -> Right ()
        [arg]
          | isMaybe home name -> checkPayload arg
          | isArray home name -> checkPayload arg
        _ ->
          Left (tipe, Error.UnsupportedType name)
    Can.TVar name ->
      Left (tipe, Error.TypeVariable name)
    Can.TLambda _ _ ->
      Left (tipe, Error.Function)
    Can.TRecord _ (Just _) ->
      Left (tipe, Error.ExtendedRecord)
    Can.TRecord fields Nothing ->
      F.traverse_ checkFieldPayload fields

checkFieldPayload :: Can.FieldType -> Either (Can.Type, Error.InvalidPayload) ()
checkFieldPayload (Can.FieldType _ tipe) =
  checkPayload tipe

isIntFloatBool :: ModuleName.Canonical -> Name.Name -> Bool
isIntFloatBool home name =
  home == ModuleName.basics
    && (name == Name.int || name == Name.float || name == Name.bool)

isUnit :: ModuleName.Canonical -> Name.Name -> Bool
isUnit home name =
  home == ModuleName.basics
    && name == Name.unit

isString :: ModuleName.Canonical -> Name.Name -> Bool
isString home name =
  home == ModuleName.string
    && name == Name.string

isJson :: ModuleName.Canonical -> Name.Name -> Bool
isJson home name =
  home == ModuleName.jsonEncode
    && name == Name.value

isMaybe :: ModuleName.Canonical -> Name.Name -> Bool
isMaybe home name =
  home == ModuleName.maybe
    && name == Name.maybe

isArray :: ModuleName.Canonical -> Name.Name -> Bool
isArray home name =
  home == ModuleName.array
    && name == Name.array
