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
import AST.SourceComments qualified as SC
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.List.NonEmpty (NonEmpty, nonEmpty)
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
    _imports :: [([Src.Comment], Src.Import)],
    _infixes :: ([Src.Comment], [A.Located Src.Infix]),
    _decls :: [Decl.Decl]
  }

chompModule :: ProjectType -> Parser E.Module Module
chompModule projectType =
  do
    header <- chompHeader
    let defaultImports = (if isCore projectType then [] else Imports.defaults)
    (imports, commentsAfterImports) <- chompImports (fmap ([],) defaultImports) []
    (infixes, commentsBeforeDecls) <-
      if isKernel projectType
        then fmap (first (commentsAfterImports,)) $ chompInfixes [] []
        else return (([], []), commentsAfterImports)
    let initialDecls =
          case nonEmpty commentsBeforeDecls of
            Nothing -> []
            Just comments -> [Decl.TopLevelComments comments]
    decls <- specialize E.Declarations $ chompDecls initialDecls
    return (Module header imports infixes decls)

-- CHECK MODULE

checkModule :: ProjectType -> Module -> Either E.Error Src.Module
checkModule projectType (Module maybeHeader imports infixes decls) =
  let (values, unions, aliases, ports, topLevelComments) = categorizeDecls [] [] [] [] [] 0 decls
   in case maybeHeader of
        Just (Header name effects exports docs comments) ->
          Src.Module (Just name) exports (toDocs docs decls) imports values unions aliases infixes topLevelComments comments
            <$> checkEffects projectType ports effects
        Nothing ->
          let comments = SC.HeaderComments [] [] [] [] [] []
           in Right $
                Src.Module Nothing (A.At A.one Src.Open) (Src.NoDocs A.one) imports values unions aliases infixes topLevelComments comments $
                  case ports of
                    [] -> Src.NoEffects
                    _ : _ -> Src.Ports ports (SC.PortsComments [])

checkEffects :: ProjectType -> [(Src.SourceOrder, Src.Port)] -> Effects -> Either E.Error Src.Effects
checkEffects projectType ports effects =
  case effects of
    NoEffects region ->
      case ports of
        [] ->
          Right Src.NoEffects
        (_, Src.Port name _) : _ ->
          case projectType of
            Package _ -> Left (E.NoPortsInPackage name)
            Application -> Left (E.UnexpectedPort region)
    Ports region comments ->
      case projectType of
        Package _ ->
          Left (E.NoPortModulesInPackage region)
        Application ->
          case ports of
            [] -> Left (E.NoPorts region)
            _ : _ -> Right (Src.Ports ports comments)
    Manager region manager comments ->
      if isKernel projectType
        then case ports of
          [] -> Right (Src.Manager region manager comments)
          _ : _ -> Left (E.UnexpectedPort region)
        else Left (E.NoEffectsOutsideKernel region)

categorizeDecls ::
  [(Src.SourceOrder, A.Located Src.Value)] ->
  [(Src.SourceOrder, A.Located Src.Union)] ->
  [(Src.SourceOrder, A.Located Src.Alias)] ->
  [(Src.SourceOrder, Src.Port)] ->
  [(Src.SourceOrder, NonEmpty Src.Comment)] ->
  Src.SourceOrder ->
  [Decl.Decl] ->
  ( [(Src.SourceOrder, A.Located Src.Value)],
    [(Src.SourceOrder, A.Located Src.Union)],
    [(Src.SourceOrder, A.Located Src.Alias)],
    [(Src.SourceOrder, Src.Port)],
    [(Src.SourceOrder, NonEmpty Src.Comment)]
  )
categorizeDecls values unions aliases ports topLevelComments index decls =
  case decls of
    [] ->
      (values, unions, aliases, ports, topLevelComments)
    decl : otherDecls ->
      case decl of
        Decl.Value _ value -> categorizeDecls ((index, value) : values) unions aliases ports topLevelComments (index + 1) otherDecls
        Decl.Union _ union -> categorizeDecls values ((index, union) : unions) aliases ports topLevelComments (index + 1) otherDecls
        Decl.Alias _ alias -> categorizeDecls values unions ((index, alias) : aliases) ports topLevelComments (index + 1) otherDecls
        Decl.Port _ port_ -> categorizeDecls values unions aliases ((index, port_) : ports) topLevelComments (index + 1) otherDecls
        Decl.TopLevelComments comments -> categorizeDecls values unions aliases ports ((index, comments) : topLevelComments) (index + 1) otherDecls

-- TO DOCS

toDocs :: Either A.Region Src.DocComment -> [Decl.Decl] -> Src.Docs
toDocs comment decls =
  case comment of
    Right overview ->
      Src.YesDocs overview (getDocComments decls [])
    Left region ->
      Src.NoDocs region

getDocComments :: [Decl.Decl] -> [(Name.Name, Src.DocComment)] -> [(Name.Name, Src.DocComment)]
getDocComments decls comments =
  case decls of
    [] ->
      comments
    decl : otherDecls ->
      case decl of
        Decl.Value c (A.At _ (Src.Value n _ _ _ _)) -> getDocComments otherDecls (addComment c n comments)
        Decl.Union c (A.At _ (Src.Union n _ _ _)) -> getDocComments otherDecls (addComment c n comments)
        Decl.Alias c (A.At _ (Src.Alias n _ _)) -> getDocComments otherDecls (addComment c n comments)
        Decl.Port c (Src.Port n _) -> getDocComments otherDecls (addComment c n comments)
        Decl.TopLevelComments _ -> getDocComments otherDecls comments

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
    ((decl, commentsAfterDecl), _) <- Decl.declaration
    let newDecls =
          case nonEmpty commentsAfterDecl of
            Nothing -> decl : decls
            Just comments -> Decl.TopLevelComments comments : decl : decls
    oneOfWithFallback
      [ do
          Space.checkFreshLine E.DeclStart
          chompDecls newDecls
      ]
      (reverse newDecls)

chompInfixes :: [A.Located Src.Infix] -> [Src.Comment] -> Parser E.Module ([A.Located Src.Infix], [Src.Comment])
chompInfixes infixes commentsBefore =
  oneOfWithFallback
    [ do
        -- TODO: use commentsBefore
        (binop, commentsAfter) <- Decl.infix_
        chompInfixes (binop : infixes) commentsAfter
    ]
    (reverse infixes, commentsBefore)

-- MODULE DOC COMMENT

chompModuleDocCommentSpace :: Parser E.Module ([Src.Comment], (Either A.Region Src.DocComment), [Src.Comment])
chompModuleDocCommentSpace =
  do
    (A.At region preComments) <- addLocation (freshLine E.FreshLine)
    oneOfWithFallback
      [ do
          docComment <- Space.docComment E.ImportStart E.ModuleSpace
          postComments <- Space.chomp E.ModuleSpace
          Space.checkFreshLine E.FreshLine
          return (preComments, (Right docComment), postComments)
      ]
      (preComments, (Left region), [])

-- HEADER

data Header
  = Header
      (A.Located Name.Name)
      Effects
      (A.Located Src.Exposing)
      (Either A.Region Src.DocComment)
      SC.HeaderComments

data Effects
  = NoEffects A.Region
  | Ports A.Region SC.PortsComments
  | Manager A.Region Src.Manager SC.ManagerComments

chompHeader :: Parser E.Module (Maybe Header)
chompHeader =
  do
    commentsBeforeModuleLine <- freshLine E.FreshLine
    start <- getPosition
    oneOfWithFallback
      [ -- module MyThing exposing (..)
        do
          Keyword.module_ E.ModuleProblem
          effectEnd <- getPosition
          commentsAfterModuleKeyword <- Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
          name <- addLocation (Var.moduleName E.ModuleName)
          commentsAfterModuleName <- Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
          Keyword.exposing_ E.ModuleProblem
          commentsAfterExposingKeyword <- Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
          exports <- addLocation (specialize E.ModuleExposing exposing)
          (commentsBeforeDocComment, docComment, commentsAfterDocComment) <- chompModuleDocCommentSpace
          let comments = SC.HeaderComments commentsBeforeModuleLine commentsAfterModuleKeyword commentsAfterModuleName commentsAfterExposingKeyword commentsBeforeDocComment commentsAfterDocComment
          return $
            Just $
              Header name (NoEffects (A.Region start effectEnd)) exports docComment comments,
        -- port module MyThing exposing (..)
        do
          Keyword.port_ E.PortModuleProblem
          commentsAfterPortKeyword <- Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
          Keyword.module_ E.PortModuleProblem
          effectEnd <- getPosition
          commentsAfterModuleKeyword <- Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
          name <- addLocation (Var.moduleName E.PortModuleName)
          commentsAfterModuleName <- Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
          Keyword.exposing_ E.PortModuleProblem
          commentsAfterExposingKeyword <- Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
          exports <- addLocation (specialize E.PortModuleExposing exposing)
          (commentsBeforeDocComment, docComment, commentsAfterDocComment) <- chompModuleDocCommentSpace
          let comments = SC.HeaderComments commentsBeforeModuleLine commentsAfterModuleKeyword commentsAfterModuleName commentsAfterExposingKeyword commentsBeforeDocComment commentsAfterDocComment
          let portsComments = SC.PortsComments commentsAfterPortKeyword
          return $
            Just $
              Header name (Ports (A.Region start effectEnd) portsComments) exports docComment comments,
        -- effect module MyThing where { command = MyCmd } exposing (..)
        do
          Keyword.effect_ E.Effect
          commentsAfterEffectKeyword <- Space.chompAndCheckIndent E.ModuleSpace E.Effect
          Keyword.module_ E.Effect
          effectEnd <- getPosition
          commentsAfterModuleKeyword <- Space.chompAndCheckIndent E.ModuleSpace E.Effect
          name <- addLocation (Var.moduleName E.ModuleName)
          commentsAfterModuleName <- Space.chompAndCheckIndent E.ModuleSpace E.Effect
          Keyword.where_ E.Effect
          commentsAfterWhereKeyword <- Space.chompAndCheckIndent E.ModuleSpace E.Effect
          (manager, commentsAfterManager1) <- chompManager
          commentsAfterManager2 <- Space.chompAndCheckIndent E.ModuleSpace E.Effect
          Keyword.exposing_ E.Effect
          commentsAfterExposingKeyword <- Space.chompAndCheckIndent E.ModuleSpace E.Effect
          exports <- addLocation (specialize (const E.Effect) exposing)
          (commentsBeforeDocComment, docComment, commentsAfterDocComment) <- chompModuleDocCommentSpace
          let comments = SC.HeaderComments commentsBeforeModuleLine commentsAfterModuleKeyword commentsAfterModuleName commentsAfterExposingKeyword commentsBeforeDocComment commentsAfterDocComment
          let managerComments = SC.ManagerComments commentsAfterEffectKeyword commentsAfterWhereKeyword (commentsAfterManager1 <> commentsAfterManager2)
          return $
            Just $
              Header name (Manager (A.Region start effectEnd) manager managerComments) exports docComment comments
      ]
      -- default header
      Nothing

chompManager :: Parser E.Module (Src.Manager, [Src.Comment])
chompManager =
  do
    word1 0x7B {- { -} E.Effect
    commentsAfterOpenBrace <- spaces_em
    oneOf
      E.Effect
      [ do
          cmd <- chompCommand
          commentsAfterCmd <- spaces_em
          oneOf
            E.Effect
            [ do
                word1 0x7D {-}-} E.Effect
                commentsAfterCloseBrace <- spaces_em
                let comments = SC.CmdComments commentsAfterOpenBrace commentsAfterCmd
                return (Src.Cmd cmd comments, commentsAfterCloseBrace),
              do
                word1 0x2C {-,-} E.Effect
                commentsAfterComma <- spaces_em
                sub <- chompSubscription
                commentsAfterSub <- spaces_em
                word1 0x7D {-}-} E.Effect
                commentsAfterCloseBrace <- spaces_em
                let cmdComments = SC.CmdComments commentsAfterOpenBrace commentsAfterCmd
                let subComments = SC.SubComments commentsAfterComma commentsAfterSub
                let comments = SC.FxComments cmdComments subComments
                return (Src.Fx cmd sub comments, commentsAfterCloseBrace)
            ],
        do
          sub <- chompSubscription
          commentsAfterSub <- spaces_em
          oneOf
            E.Effect
            [ do
                word1 0x7D {-}-} E.Effect
                commentsAfterCloseBrace <- spaces_em
                let comments = SC.SubComments commentsAfterOpenBrace commentsAfterSub
                return (Src.Sub sub comments, commentsAfterCloseBrace),
              do
                word1 0x2C {-,-} E.Effect
                commentsAfterComma <- spaces_em
                cmd <- chompCommand
                commentsAfterCmd <- spaces_em
                word1 0x7D {-}-} E.Effect
                commentsAfterCloseBrace <- spaces_em
                let subComments = SC.SubComments commentsAfterOpenBrace commentsAfterSub
                let cmdComments = SC.CmdComments commentsAfterComma commentsAfterCmd
                let comments = SC.FxComments cmdComments subComments
                return (Src.Fx cmd sub comments, commentsAfterCloseBrace)
            ]
      ]

chompCommand :: Parser E.Module (A.Located Name.Name)
chompCommand =
  do
    Keyword.command_ E.Effect
    spaces_em
    word1 0x3D {-=-} E.Effect
    spaces_em
    addLocation (Var.upper E.Effect)

chompSubscription :: Parser E.Module (A.Located Name.Name)
chompSubscription =
  do
    Keyword.subscription_ E.Effect
    spaces_em
    word1 0x3D {-=-} E.Effect
    spaces_em
    addLocation (Var.upper E.Effect)

spaces_em :: Parser E.Module [Src.Comment]
spaces_em =
  Space.chompAndCheckIndent E.ModuleSpace E.Effect

-- IMPORTS

chompImports :: [([Src.Comment], Src.Import)] -> [Src.Comment] -> Parser E.Module ([([Src.Comment], Src.Import)], [Src.Comment])
chompImports is trailingComments =
  oneOfWithFallback
    [ do
        (i, commentsAfterI) <- chompImport
        chompImports ((trailingComments, i) : is) commentsAfterI
    ]
    (reverse is, trailingComments)

chompImport :: Parser E.Module (Src.Import, [Src.Comment])
chompImport =
  do
    Keyword.import_ E.ImportStart
    commentsAfterImportKeyword <- Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentName
    name@(A.At (A.Region _ end) _) <- addLocation (Var.moduleName E.ImportName)
    commentsAfterName <- Space.chompIndentedMoreThan 1 E.ModuleSpace
    outdentedComments <- Space.chomp E.ModuleSpace
    oneOf
      E.ImportEnd
      [ do
          Space.checkFreshLine E.ImportEnd
          let comments = SC.ImportComments commentsAfterImportKeyword commentsAfterName
          return $ (Src.Import name Nothing (Src.Explicit []) Nothing comments, outdentedComments),
        do
          Space.checkIndent end E.ImportEnd
          let comments = SC.ImportComments commentsAfterImportKeyword (commentsAfterName ++ outdentedComments)
          oneOf
            E.ImportAs
            [ chompAs name comments,
              chompExposing name Nothing comments
            ]
      ]

chompAs :: A.Located Name.Name -> SC.ImportComments -> Parser E.Module (Src.Import, [Src.Comment])
chompAs name comments =
  do
    Keyword.as_ E.ImportAs
    commentsAfterAs <- Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentAlias
    alias <- Var.moduleName E.ImportAlias
    end <- getPosition
    commentsAfterAliasName <- Space.chompIndentedMoreThan 1 E.ModuleSpace
    outdentedComments <- Space.chomp E.ModuleSpace
    oneOf
      E.ImportEnd
      [ do
          Space.checkFreshLine E.ImportEnd
          let aliasComments = SC.ImportAliasComments commentsAfterAs commentsAfterAliasName
          return (Src.Import name (Just (alias, aliasComments)) (Src.Explicit []) Nothing comments, outdentedComments),
        do
          Space.checkIndent end E.ImportEnd
          let aliasComments = SC.ImportAliasComments commentsAfterAs (commentsAfterAliasName <> outdentedComments)
          chompExposing name (Just (alias, aliasComments)) comments
      ]

chompExposing :: A.Located Name.Name -> Maybe (Name.Name, SC.ImportAliasComments) -> SC.ImportComments -> Parser E.Module (Src.Import, [Src.Comment])
chompExposing name maybeAlias comments =
  do
    Keyword.exposing_ E.ImportExposing
    commentsAfterExposing <- Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentExposingArray
    exposed <- specialize E.ImportExposingArray exposing
    commentsAfterListing <- Space.chompIndentedMoreThan 1 E.ModuleSpace
    outdentedComments <- freshLine E.ImportEnd
    let exposingComments = SC.ImportExposingComments commentsAfterExposing commentsAfterListing
    return (Src.Import name maybeAlias exposed (Just exposingComments) comments, outdentedComments)

-- LISTING

exposing :: Parser E.Exposing Src.Exposing
exposing =
  do
    word1 0x28 {-(-} E.ExposingStart
    Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue
    oneOf
      E.ExposingValue
      [ do
          word2 0x2E 0x2E {-..-} E.ExposingValue
          Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
          word1 0x29 {-)-} E.ExposingEnd
          return Src.Open,
        do
          exposed <- chompExposed
          Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
          exposingHelp [exposed]
      ]

exposingHelp :: [Src.Exposed] -> Parser E.Exposing Src.Exposing
exposingHelp revExposed =
  oneOf
    E.ExposingEnd
    [ do
        word1 0x2C {-,-} E.ExposingEnd
        Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue
        exposed <- chompExposed
        Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
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
          Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
          Src.Upper (A.at start end name) <$> privacy
      ]

privacy :: Parser E.Exposing Src.Privacy
privacy =
  oneOfWithFallback
    [ do
        word1 0x28 {-(-} E.ExposingTypePrivacy
        Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy
        start <- getPosition
        word2 0x2E 0x2E {-..-} E.ExposingTypePrivacy
        end <- getPosition
        Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy
        word1 0x29 {-)-} E.ExposingTypePrivacy
        return $ Src.Public (A.Region start end)
    ]
    Src.Private
