{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Reporting.Render.Type
  ( Context (..),
    lambda,
    apply,
    record,
    vrecordSnippet,
    vrecord,
    srcToDoc,
    canToDoc,
  )
where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Data.Name qualified as Name
import Reporting.Annotation qualified as A
import Reporting.Doc (Doc, (<+>))
import Reporting.Doc qualified as D
import Reporting.Render.Type.Localizer qualified as L

-- TO DOC

data Context
  = None
  | Func
  | App

lambda :: Context -> Doc -> Doc -> [Doc] -> Doc
lambda context arg1 arg2 args =
  let lambdaDoc =
        D.align $ D.sep (arg1 : map ("->" <+>) (arg2 : args))
   in case context of
        None -> lambdaDoc
        Func -> D.cat ["(", lambdaDoc, ")"]
        App -> D.cat ["(", lambdaDoc, ")"]

apply :: Context -> Doc -> [Doc] -> Doc
apply context name args =
  case args of
    [] ->
      name
    _ : _ ->
      let applyDoc =
            D.hang 4 (D.sep (name : args))
       in case context of
            App -> D.cat ["(", applyDoc, ")"]
            Func -> applyDoc
            None -> applyDoc

record :: [(Doc, Doc)] -> Maybe Doc -> Doc
record entries maybeExt =
  case (map entryToDoc entries, maybeExt) of
    ([], Nothing) ->
      "{}"
    (fields, Nothing) ->
      D.align $
        D.sep $
          [ D.cat (zipWith (<+>) ("{" : repeat ",") fields),
            "}"
          ]
    (fields, Just ext) ->
      D.align $
        D.sep $
          [ D.hang 4 $
              D.sep $
                [ "{" <+> ext,
                  D.cat (zipWith (<+>) ("|" : repeat ",") fields)
                ],
            "}"
          ]

entryToDoc :: (Doc, Doc) -> Doc
entryToDoc (fieldName, fieldType) =
  D.hang 4 (D.sep [fieldName <+> ":", fieldType])

vrecordSnippet :: (Doc, Doc) -> [(Doc, Doc)] -> Doc
vrecordSnippet entry entries =
  let field = "{" <+> entryToDoc entry
      fields = zipWith (<+>) (repeat ",") (map entryToDoc entries ++ ["..."])
   in D.vcat (field : fields ++ ["}"])

vrecord :: [(Doc, Doc)] -> Maybe Doc -> Doc
vrecord entries maybeExt =
  case (map entryToDoc entries, maybeExt) of
    ([], Nothing) ->
      "{}"
    (fields, Nothing) ->
      D.vcat $
        zipWith (<+>) ("{" : repeat ",") fields ++ ["}"]
    (fields, Just ext) ->
      D.vcat
        [ D.hang 4 $
            D.vcat $
              [ "{" <+> ext,
                D.cat (zipWith (<+>) ("|" : repeat ",") fields)
              ],
          "}"
        ]

-- SOURCE TYPE TO DOC

srcToDoc :: Context -> Src.Type -> Doc
srcToDoc context (A.At _ tipe) =
  case tipe of
    Src.TLambda arg1 result ->
      let (arg2, rest) = collectSrcArgs result
       in lambda
            context
            (srcToDoc Func arg1)
            (srcToDoc Func arg2)
            (map (srcToDoc Func) rest)
    Src.TVar name ->
      D.fromName name
    Src.TType _ name args ->
      apply
        context
        (D.fromName name)
        (map (srcToDoc App . snd) args)
    Src.TTypeQual _ home name args ->
      apply
        context
        (D.fromName home <> "." <> D.fromName name)
        (map (srcToDoc App . snd) args)
    Src.TRecord fields ext ->
      record
        (map srcFieldToDocs fields)
        (fmap (D.fromName . A.toValue) ext)

srcFieldToDocs :: Src.TRecordField -> (Doc, Doc)
srcFieldToDocs (A.At _ fieldName, fieldType, _) =
  ( D.fromName fieldName,
    srcToDoc None fieldType
  )

collectSrcArgs :: Src.Type -> (Src.Type, [Src.Type])
collectSrcArgs tipe =
  case tipe of
    A.At _ (Src.TLambda a result) ->
      let (b, cs) = collectSrcArgs result
       in (a, b : cs)
    _ ->
      (tipe, [])

-- CANONICAL TYPE TO DOC

canToDoc :: L.Localizer -> Context -> Can.Type -> Doc
canToDoc localizer context tipe =
  case tipe of
    Can.TLambda arg1 result ->
      let (arg2, rest) = collectArgs result
       in lambda
            context
            (canToDoc localizer Func arg1)
            (canToDoc localizer Func arg2)
            (map (canToDoc localizer Func) rest)
    Can.TVar name ->
      D.fromName name
    Can.TType home name args ->
      apply
        context
        (L.toDoc localizer home name)
        (map (canToDoc localizer App) args)
    Can.TRecord fields ext ->
      record
        (map (canFieldToDoc localizer) (Can.fieldsToList fields))
        (fmap D.fromName ext)
    Can.TAlias home name args _ ->
      apply
        context
        (L.toDoc localizer home name)
        (map (canToDoc localizer App . snd) args)

canFieldToDoc :: L.Localizer -> (Name.Name, Can.Type) -> (Doc, Doc)
canFieldToDoc localizer (name, tipe) =
  ( D.fromName name,
    canToDoc localizer None tipe
  )

collectArgs :: Can.Type -> (Can.Type, [Can.Type])
collectArgs tipe =
  case tipe of
    Can.TLambda a rest ->
      let (b, cs) = collectArgs rest
       in (a, b : cs)
    _ ->
      (tipe, [])
