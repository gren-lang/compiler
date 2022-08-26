{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
-- Temporary while implementing gren format
{-# OPTIONS_GHC -Wno-error=unused-do-bind #-}
{-# OPTIONS_GHC -Wno-error=unused-matches #-}

module Parse.Module
  ( fromByteString,
    ProjectType (..),
    isKernel,
    chompImports,
    chompImport,
  )
where

import AST.Source qualified as Src
import Data.ByteString qualified as BS
import Data.Name qualified as Name
import Gren.Compiler.Imports qualified as Imports
import Gren.Package qualified as Pkg
import Parse.Declaration qualified as Decl
import Parse.Keyword qualified as Keyword
import Parse.Primitives hiding (State, fromByteString)
import Parse.Primitives qualified as P
import Parse.Space qualified as Space
import Parse.Symbol qualified as Symbol
import Parse.Variable qualified as Var
import Reporting.Annotation qualified as A
import Reporting.Error.Syntax qualified as E

-- FROM BYTE STRING

fromByteString :: ProjectType -> BS.ByteString -> Either E.Error Src.Module
fromByteString projectType source =
  case P.fromByteString (chompModule projectType) E.ModuleBadEnd source of
    Right modul -> checkModule projectType modul
    Left err -> Left (E.ParseError err)

-- PROJECT TYPE

data ProjectType
  = Package Pkg.Name
  | Application

isCore :: ProjectType -> Bool
isCore projectType =
  case projectType of
    Package pkg -> pkg == Pkg.core
    Application -> False

isKernel :: ProjectType -> Bool
isKernel projectType =
  case projectType of
    Package pkg -> Pkg.isKernel pkg
    Application -> False

-- MODULE

data Module = Module
  { _header :: Maybe Header,
    _imports :: [Src.Import],
    _infixes :: [A.Located Src.Infix],
    _decls :: [Decl.Decl]
  }

chompModule :: ProjectType -> Parser E.Module Module
chompModule projectType =
  do
    header <- chompHeader
    imports <- chompImports (if isCore projectType then [] else Imports.defaults)
    infixes <- if isKernel projectType then chompInfixes [] else return []
    decls <- specialize E.Declarations $ chompDecls []
    return (Module header imports infixes decls)

-- CHECK MODULE

checkModule :: ProjectType -> Module -> Either E.Error Src.Module
checkModule projectType (Module maybeHeader imports infixes decls) =
  let (values, unions, aliases, ports) = categorizeDecls [] [] [] [] decls
   in case maybeHeader of
        Just (Header name effects exports docs) ->
          Src.Module (Just name) exports (toDocs docs decls) imports values unions aliases infixes
            <$> checkEffects projectType ports effects
        Nothing ->
          Right $
            Src.Module Nothing (A.At A.one Src.Open) (Src.NoDocs A.one) imports values unions aliases infixes $
              case ports of
                [] -> Src.NoEffects
                _ : _ -> Src.Ports ports

checkEffects :: ProjectType -> [Src.Port] -> Effects -> Either E.Error Src.Effects
checkEffects projectType ports effects =
  case effects of
    NoEffects region ->
      case ports of
        [] ->
          Right Src.NoEffects
        Src.Port name _ : _ ->
          case projectType of
            Package _ -> Left (E.NoPortsInPackage name)
            Application -> Left (E.UnexpectedPort region)
    Ports region ->
      case projectType of
        Package _ ->
          Left (E.NoPortModulesInPackage region)
        Application ->
          case ports of
            [] -> Left (E.NoPorts region)
            _ : _ -> Right (Src.Ports ports)
    Manager region manager ->
      if isKernel projectType
        then case ports of
          [] -> Right (Src.Manager region manager)
          _ : _ -> Left (E.UnexpectedPort region)
        else Left (E.NoEffectsOutsideKernel region)

categorizeDecls :: [A.Located Src.Value] -> [A.Located Src.Union] -> [A.Located Src.Alias] -> [Src.Port] -> [Decl.Decl] -> ([A.Located Src.Value], [A.Located Src.Union], [A.Located Src.Alias], [Src.Port])
categorizeDecls values unions aliases ports decls =
  case decls of
    [] ->
      (values, unions, aliases, ports)
    decl : otherDecls ->
      case decl of
        Decl.Value _ value -> categorizeDecls (value : values) unions aliases ports otherDecls
        Decl.Union _ union -> categorizeDecls values (union : unions) aliases ports otherDecls
        Decl.Alias _ alias -> categorizeDecls values unions (alias : aliases) ports otherDecls
        Decl.Port _ port_ -> categorizeDecls values unions aliases (port_ : ports) otherDecls

-- TO DOCS

toDocs :: Either A.Region Src.DocComment -> [Decl.Decl] -> Src.Docs
toDocs comment decls =
  case comment of
    Right overview ->
      Src.YesDocs overview (getComments decls [])
    Left region ->
      Src.NoDocs region

getComments :: [Decl.Decl] -> [(Name.Name, Src.DocComment)] -> [(Name.Name, Src.DocComment)]
getComments decls comments =
  case decls of
    [] ->
      comments
    decl : otherDecls ->
      case decl of
        Decl.Value c (A.At _ (Src.Value n _ _ _)) -> getComments otherDecls (addComment c n comments)
        Decl.Union c (A.At _ (Src.Union n _ _)) -> getComments otherDecls (addComment c n comments)
        Decl.Alias c (A.At _ (Src.Alias n _ _)) -> getComments otherDecls (addComment c n comments)
        Decl.Port c (Src.Port n _) -> getComments otherDecls (addComment c n comments)

addComment :: Maybe Src.DocComment -> A.Located Name.Name -> [(Name.Name, Src.DocComment)] -> [(Name.Name, Src.DocComment)]
addComment maybeComment (A.At _ name) comments =
  case maybeComment of
    Just comment -> (name, comment) : comments
    Nothing -> comments

-- FRESH LINES

freshLine :: (Row -> Col -> E.Module) -> Parser E.Module [Src.Comment]
freshLine toFreshLineError =
  do
    comments <- Space.chomp E.ModuleSpace
    Space.checkFreshLine toFreshLineError
    return comments

-- CHOMP DECLARATIONS

chompDecls :: [Decl.Decl] -> Parser E.Decl [Decl.Decl]
chompDecls decls =
  do
    (decl, _) <- Decl.declaration
    oneOfWithFallback
      [ do
          Space.checkFreshLine E.DeclStart
          chompDecls (decl : decls)
      ]
      (reverse (decl : decls))

chompInfixes :: [A.Located Src.Infix] -> Parser E.Module [A.Located Src.Infix]
chompInfixes infixes =
  oneOfWithFallback
    [ do
        binop <- Decl.infix_
        chompInfixes (binop : infixes)
    ]
    infixes

-- MODULE DOC COMMENT

chompModuleDocCommentSpace :: Parser E.Module (Either A.Region Src.DocComment)
chompModuleDocCommentSpace =
  do
    (A.At region _comments) <- addLocation (freshLine E.FreshLine)
    oneOfWithFallback
      [ do
          docComment <- Space.docComment E.ImportStart E.ModuleSpace
          _ <- Space.chomp E.ModuleSpace
          Space.checkFreshLine E.FreshLine
          return (Right docComment)
      ]
      (Left region)

-- HEADER

data Header
  = Header (A.Located Name.Name) Effects (A.Located Src.Exposing) (Either A.Region Src.DocComment)

data Effects
  = NoEffects A.Region
  | Ports A.Region
  | Manager A.Region Src.Manager

chompHeader :: Parser E.Module (Maybe Header)
chompHeader =
  do
    _ <- freshLine E.FreshLine
    start <- getPosition
    oneOfWithFallback
      [ -- module MyThing exposing (..)
        do
          Keyword.module_ E.ModuleProblem
          effectEnd <- getPosition
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
          name <- addLocation (Var.moduleName E.ModuleName)
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
          Keyword.exposing_ E.ModuleProblem
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
          exports <- addLocation (specialize E.ModuleExposing exposing)
          comment <- chompModuleDocCommentSpace
          return $
            Just $
              Header name (NoEffects (A.Region start effectEnd)) exports comment,
        -- port module MyThing exposing (..)
        do
          Keyword.port_ E.PortModuleProblem
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
          Keyword.module_ E.PortModuleProblem
          effectEnd <- getPosition
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
          name <- addLocation (Var.moduleName E.PortModuleName)
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
          Keyword.exposing_ E.PortModuleProblem
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
          exports <- addLocation (specialize E.PortModuleExposing exposing)
          comment <- chompModuleDocCommentSpace
          return $
            Just $
              Header name (Ports (A.Region start effectEnd)) exports comment,
        -- effect module MyThing where { command = MyCmd } exposing (..)
        do
          Keyword.effect_ E.Effect
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.Effect
          Keyword.module_ E.Effect
          effectEnd <- getPosition
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.Effect
          name <- addLocation (Var.moduleName E.ModuleName)
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.Effect
          Keyword.where_ E.Effect
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.Effect
          manager <- chompManager
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.Effect
          Keyword.exposing_ E.Effect
          _ <- Space.chompAndCheckIndent E.ModuleSpace E.Effect
          exports <- addLocation (specialize (const E.Effect) exposing)
          comment <- chompModuleDocCommentSpace
          return $
            Just $
              Header name (Manager (A.Region start effectEnd) manager) exports comment
      ]
      -- default header
      Nothing

chompManager :: Parser E.Module Src.Manager
chompManager =
  do
    word1 0x7B {- { -} E.Effect
    _ <- spaces_em
    oneOf
      E.Effect
      [ do
          cmd <- chompCommand
          _ <- spaces_em
          oneOf
            E.Effect
            [ do
                word1 0x7D {-}-} E.Effect
                _ <- spaces_em
                return (Src.Cmd cmd),
              do
                word1 0x2C {-,-} E.Effect
                _ <- spaces_em
                sub <- chompSubscription
                _ <- spaces_em
                word1 0x7D {-}-} E.Effect
                _ <- spaces_em
                return (Src.Fx cmd sub)
            ],
        do
          sub <- chompSubscription
          _ <- spaces_em
          oneOf
            E.Effect
            [ do
                word1 0x7D {-}-} E.Effect
                _ <- spaces_em
                return (Src.Sub sub),
              do
                word1 0x2C {-,-} E.Effect
                _ <- spaces_em
                cmd <- chompCommand
                _ <- spaces_em
                word1 0x7D {-}-} E.Effect
                _ <- spaces_em
                return (Src.Fx cmd sub)
            ]
      ]

chompCommand :: Parser E.Module (A.Located Name.Name)
chompCommand =
  do
    Keyword.command_ E.Effect
    _ <- spaces_em
    word1 0x3D {-=-} E.Effect
    _ <- spaces_em
    addLocation (Var.upper E.Effect)

chompSubscription :: Parser E.Module (A.Located Name.Name)
chompSubscription =
  do
    Keyword.subscription_ E.Effect
    _ <- spaces_em
    word1 0x3D {-=-} E.Effect
    _ <- spaces_em
    addLocation (Var.upper E.Effect)

spaces_em :: Parser E.Module [Src.Comment]
spaces_em =
  Space.chompAndCheckIndent E.ModuleSpace E.Effect

-- IMPORTS

chompImports :: [Src.Import] -> Parser E.Module [Src.Import]
chompImports is =
  oneOfWithFallback
    [ do
        i <- chompImport
        chompImports (i : is)
    ]
    (reverse is)

chompImport :: Parser E.Module Src.Import
chompImport =
  do
    Keyword.import_ E.ImportStart
    _ <- Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentName
    name@(A.At (A.Region _ end) _) <- addLocation (Var.moduleName E.ImportName)
    _ <- Space.chomp E.ModuleSpace
    oneOf
      E.ImportEnd
      [ do
          Space.checkFreshLine E.ImportEnd
          return $ Src.Import name Nothing (Src.Explicit []),
        do
          Space.checkIndent end E.ImportEnd
          oneOf
            E.ImportAs
            [ chompAs name,
              chompExposing name Nothing
            ]
      ]

chompAs :: A.Located Name.Name -> Parser E.Module Src.Import
chompAs name =
  do
    Keyword.as_ E.ImportAs
    _ <- Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentAlias
    alias <- Var.upper E.ImportAlias
    end <- getPosition
    _ <- Space.chomp E.ModuleSpace
    oneOf
      E.ImportEnd
      [ do
          Space.checkFreshLine E.ImportEnd
          return $ Src.Import name (Just alias) (Src.Explicit []),
        do
          Space.checkIndent end E.ImportEnd
          chompExposing name (Just alias)
      ]

chompExposing :: A.Located Name.Name -> Maybe Name.Name -> Parser E.Module Src.Import
chompExposing name maybeAlias =
  do
    Keyword.exposing_ E.ImportExposing
    _ <- Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentExposingArray
    exposed <- specialize E.ImportExposingArray exposing
    _ <- freshLine E.ImportEnd
    return $ Src.Import name maybeAlias exposed

-- LISTING

exposing :: Parser E.Exposing Src.Exposing
exposing =
  do
    word1 0x28 {-(-} E.ExposingStart
    _ <- Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue
    oneOf
      E.ExposingValue
      [ do
          word2 0x2E 0x2E {-..-} E.ExposingValue
          _ <- Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
          word1 0x29 {-)-} E.ExposingEnd
          return Src.Open,
        do
          exposed <- chompExposed
          _ <- Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
          exposingHelp [exposed]
      ]

exposingHelp :: [Src.Exposed] -> Parser E.Exposing Src.Exposing
exposingHelp revExposed =
  oneOf
    E.ExposingEnd
    [ do
        word1 0x2C {-,-} E.ExposingEnd
        _ <- Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue
        exposed <- chompExposed
        _ <- Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
        exposingHelp (exposed : revExposed),
      do
        word1 0x29 {-)-} E.ExposingEnd
        return (Src.Explicit (reverse revExposed))
    ]

chompExposed :: Parser E.Exposing Src.Exposed
chompExposed =
  do
    start <- getPosition
    oneOf
      E.ExposingValue
      [ do
          name <- Var.lower E.ExposingValue
          end <- getPosition
          return $ Src.Lower $ A.at start end name,
        do
          word1 0x28 {-(-} E.ExposingValue
          op <- Symbol.operator E.ExposingOperator E.ExposingOperatorReserved
          word1 0x29 {-)-} E.ExposingOperatorRightParen
          end <- getPosition
          return $ Src.Operator (A.Region start end) op,
        do
          name <- Var.upper E.ExposingValue
          end <- getPosition
          _ <- Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
          Src.Upper (A.at start end name) <$> privacy
      ]

privacy :: Parser E.Exposing Src.Privacy
privacy =
  oneOfWithFallback
    [ do
        word1 0x28 {-(-} E.ExposingTypePrivacy
        _ <- Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy
        start <- getPosition
        word2 0x2E 0x2E {-..-} E.ExposingTypePrivacy
        end <- getPosition
        _ <- Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy
        word1 0x29 {-)-} E.ExposingTypePrivacy
        return $ Src.Public (A.Region start end)
    ]
    Src.Private
