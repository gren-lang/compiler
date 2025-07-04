{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Repl
  ( Flags (..),
    run,
    --
    Lines (..),
    Input (..),
    Prefill (..),
    CategorizedInput (..),
    categorize,
    --
    State (..),
    Output (..),
    toByteString,
  )
where

import AST.Source qualified as Src
import Build qualified
import Control.Applicative ((<|>))
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans (lift, liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Internal (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.UTF8 qualified as BS_UTF8
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Name qualified as N
import Directories qualified as Dirs
import Generate qualified
import Gren.Details qualified as Details
import Gren.ModuleName qualified as ModuleName
import Gren.Outline (Outline)
import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import Parse.Declaration qualified as PD
import Parse.Expression qualified as PE
import Parse.Module qualified as PM
import Parse.Primitives (Col, Row)
import Parse.Primitives qualified as P
import Parse.Space qualified as PS
import Parse.Type qualified as PT
import Parse.Variable qualified as PV
import Reporting.Annotation qualified as A
import Reporting.Doc ((<+>))
import Reporting.Doc qualified as D
import Reporting.Error.Syntax qualified as ES
import Reporting.Exit qualified as Exit
import Reporting.Render.Code qualified as Code
import Reporting.Report qualified as Report
import Reporting.Task qualified as Task
import System.Console.Haskeline qualified as Repl
import System.Directory qualified as Dir
import System.Environment qualified
import System.Exit qualified as Exit
import System.FilePath ((</>))
import System.IO qualified as IO
import System.Process qualified as Proc
import Prelude hiding (lines, read)

-- RUN

data Flags = Flags
  { _maybeInterpreter :: Maybe FilePath,
    _root :: FilePath,
    _outline :: Outline,
    _root_sources :: Map ModuleName.Raw ByteString,
    _dependencies :: Map Pkg.Name Details.Dependency
  }

run :: Flags -> IO ()
run flags =
  do
    settings <- initSettings
    env <- initEnv flags
    printWelcomeMessage
    let looper = Repl.runInputT settings (Repl.withInterrupt (loop flags env initialState))
    exitCode <- State.evalStateT looper initialState
    Exit.exitWith exitCode

-- WELCOME

printWelcomeMessage :: IO ()
printWelcomeMessage =
  let vsn = V.toChars V.compiler
      title = "Gren" <+> D.fromChars vsn
      dashes = replicate (70 - length vsn) '-'
   in D.toAnsi IO.stdout $
        D.vcat
          [ D.black "----" <+> D.dullcyan title <+> D.black (D.fromChars dashes),
            D.black $ D.fromChars $ "Say :help for help and :exit to exit!",
            D.black "--------------------------------------------------------------------------------",
            D.empty
          ]

-- ENV

data Env = Env
  { _interpreter :: FilePath,
    _ansi :: Bool
  }

initEnv :: Flags -> IO Env
initEnv (Flags maybeAlternateInterpreter _ _ _ _) =
  do
    noColorsEnv <- System.Environment.lookupEnv "NO_COLOR"
    let noColors = Maybe.isJust noColorsEnv
    interpreter <- getInterpreter maybeAlternateInterpreter
    return $ Env interpreter (not noColors)

-- LOOP

data Outcome
  = Loop State
  | End Exit.ExitCode

type M =
  State.StateT State IO

loop :: Flags -> Env -> State -> Repl.InputT M Exit.ExitCode
loop flags env state =
  do
    input <- Repl.handleInterrupt (return Skip) read
    outcome <- liftIO (eval flags env state input)
    case outcome of
      Loop state ->
        do
          lift (State.put state)
          loop flags env state
      End exitCode ->
        return exitCode

-- READ

data Input
  = Import ModuleName.Raw BS.ByteString
  | Type N.Name BS.ByteString
  | Port
  | Decl N.Name BS.ByteString
  | Expr BS.ByteString
  | --
    Reset
  | Exit
  | Skip
  | Help (Maybe String)

read :: Repl.InputT M Input
read =
  do
    maybeLine <- Repl.getInputLine "> "
    case maybeLine of
      Nothing ->
        return Exit
      Just chars ->
        let lines = Lines chars []
         in case categorize lines of
              Done input -> return input
              Continue p -> readMore lines p

readMore :: Lines -> Prefill -> Repl.InputT M Input
readMore previousLines prefill =
  do
    input <- Repl.getInputLineWithInitial "| " (renderPrefill prefill, "")
    case input of
      Nothing ->
        return Skip
      Just chars ->
        let lines = addLine chars previousLines
         in case categorize lines of
              Done input -> return input
              Continue p -> readMore lines p

data Prefill
  = Indent
  | DefStart N.Name

renderPrefill :: Prefill -> String
renderPrefill lineStart =
  case lineStart of
    Indent ->
      "  "
    DefStart name ->
      N.toChars name ++ " "

-- LINES

data Lines = Lines
  { _prevLine :: String,
    _revLines :: [String]
  }

addLine :: [Char] -> Lines -> Lines
addLine line (Lines x xs) =
  Lines line (x : xs)

isBlank :: Lines -> Bool
isBlank (Lines prev rev) =
  null rev && all (== ' ') prev

isSingleLine :: Lines -> Bool
isSingleLine (Lines _ rev) =
  null rev

endsWithBlankLine :: Lines -> Bool
endsWithBlankLine (Lines prev _) =
  all (== ' ') prev

linesToByteString :: Lines -> BS_UTF8.ByteString
linesToByteString (Lines prev rev) =
  BS_UTF8.fromString (unlines (reverse (prev : rev)))

getFirstLine :: Lines -> String
getFirstLine (Lines x xs) =
  case xs of
    [] -> x
    y : ys -> getFirstLine (Lines y ys)

-- CATEGORIZE INPUT

data CategorizedInput
  = Done Input
  | Continue Prefill

categorize :: Lines -> CategorizedInput
categorize lines
  | isBlank lines = Done Skip
  | startsWithColon lines = Done (toCommand lines)
  | startsWithKeyword "import" lines = attemptImport lines
  | otherwise = attemptDeclOrExpr lines

attemptImport :: Lines -> CategorizedInput
attemptImport lines =
  let src = linesToByteString lines
      parser = P.specialize (\_ _ _ -> ()) PM.chompImport
   in case P.fromByteString parser (\_ _ -> ()) src of
        Right (Src.Import (A.At _ name) _ _ _ _, _) ->
          Done (Import name src)
        Left () ->
          ifFail lines (Import "ERR" src)

ifFail :: Lines -> Input -> CategorizedInput
ifFail lines input =
  if endsWithBlankLine lines
    then Done input
    else Continue Indent

ifDone :: Lines -> Input -> CategorizedInput
ifDone lines input =
  if isSingleLine lines || endsWithBlankLine lines
    then Done input
    else Continue Indent

attemptDeclOrExpr :: Lines -> CategorizedInput
attemptDeclOrExpr lines =
  let src = linesToByteString lines
      exprParser = P.specialize (toExprPosition src) PE.expression
      declParser = P.specialize (toDeclPosition src) PD.declaration
   in case P.fromByteString declParser (,) src of
        Right ((decl, _), _) ->
          case decl of
            PD.Value _ (A.At _ (Src.Value (A.At _ name) _ _ _ _)) -> ifDone lines (Decl name src)
            PD.Union _ (A.At _ (Src.Union (A.At _ name) _ _ _)) -> ifDone lines (Type name src)
            PD.Alias _ (A.At _ (Src.Alias (A.At _ name) _ _)) -> ifDone lines (Type name src)
            PD.Port _ _ -> Done Port
            PD.TopLevelComments _ -> Done Skip
        Left declPosition
          | startsWithKeyword "type" lines ->
              ifFail lines (Type "ERR" src)
          | startsWithKeyword "port" lines ->
              Done Port
          | otherwise ->
              case P.fromByteString exprParser (,) src of
                Right _ ->
                  ifDone lines (Expr src)
                Left exprPosition ->
                  if exprPosition >= declPosition
                    then ifFail lines (Expr src)
                    else case P.fromByteString annotation (\_ _ -> ()) src of
                      Right name -> Continue (DefStart name)
                      Left () -> ifFail lines (Decl "ERR" src)

startsWithColon :: Lines -> Bool
startsWithColon lines =
  case dropWhile (== ' ') (getFirstLine lines) of
    [] -> False
    c : _ -> c == ':'

toCommand :: Lines -> Input
toCommand lines =
  case drop 1 $ dropWhile (== ' ') (getFirstLine lines) of
    "reset" -> Reset
    "exit" -> Exit
    "quit" -> Exit
    "help" -> Help Nothing
    rest -> Help (Just (takeWhile (/= ' ') rest))

startsWithKeyword :: [Char] -> Lines -> Bool
startsWithKeyword keyword lines =
  let line = getFirstLine lines
   in List.isPrefixOf keyword line
        && case drop (length keyword) line of
          [] -> True
          c : _ -> not (Char.isAlphaNum c)

toExprPosition :: BS.ByteString -> ES.Expr -> Row -> Col -> (Row, Col)
toExprPosition src expr row col =
  let decl = ES.DeclDef N.replValueToPrint (ES.DeclDefBody expr row col) row col
   in toDeclPosition src decl row col

toDeclPosition :: BS.ByteString -> ES.Decl -> Row -> Col -> (Row, Col)
toDeclPosition src decl r c =
  let err = ES.ParseError (ES.Declarations decl r c)
      report = ES.toReport (Code.toSource src) err

      (Report.Report _ (A.Region (A.Position row col) _) _ _) = report
   in (row, col)

annotation :: P.Parser () N.Name
annotation =
  let err _ _ = ()
      err_ _ _ _ = ()
   in do
        name <- PV.lower err
        _ <- PS.chompAndCheckIndent err_ err
        P.word1 0x3A {-:-} err
        _ <- PS.chompAndCheckIndent err_ err
        (_, _) <- P.specialize err_ PT.expression
        PS.checkFreshLine err
        return name

-- STATE

data State = State
  { _imports :: Map.Map N.Name B.Builder,
    _types :: Map.Map N.Name B.Builder,
    _decls :: Map.Map N.Name B.Builder
  }

initialState :: State
initialState =
  State Map.empty Map.empty Map.empty

-- EVAL

eval :: Flags -> Env -> State -> Input -> IO Outcome
eval flags env state@(State imports types decls) input =
  Repl.handleInterrupt (putStrLn "<cancelled>" >> return (Loop state)) $
    case input of
      Skip ->
        return (Loop state)
      Exit ->
        return (End Exit.ExitSuccess)
      Reset ->
        do
          putStrLn "<reset>"
          return (Loop initialState)
      Help maybeUnknownCommand ->
        do
          putStrLn (toHelpMessage maybeUnknownCommand)
          return (Loop state)
      Import name src ->
        do
          let newState = state {_imports = Map.insert name (B.byteString src) imports}
          Loop <$> attemptEval flags env state newState OutputNothing
      Type name src ->
        do
          let newState = state {_types = Map.insert name (B.byteString src) types}
          Loop <$> attemptEval flags env state newState OutputNothing
      Port ->
        do
          putStrLn "I cannot handle port declarations."
          return (Loop state)
      Decl name src ->
        do
          let newState = state {_decls = Map.insert name (B.byteString src) decls}
          Loop <$> attemptEval flags env state newState (OutputDecl name)
      Expr src ->
        Loop <$> attemptEval flags env state state (OutputExpr src)

-- ATTEMPT EVAL

data Output
  = OutputNothing
  | OutputDecl N.Name
  | OutputExpr BS.ByteString

attemptEval :: Flags -> Env -> State -> State -> Output -> IO State
attemptEval (Flags _ root outline sources deps) (Env interpreter ansi) oldState newState output =
  do
    result <-
      Task.run $
        do
          details <-
            Task.eio Exit.ReplBadDetails $
              Details.load outline deps

          artifacts <-
            Task.eio id $
              Build.fromRepl root details sources (toByteString newState output)

          traverse (Task.mapError Exit.ReplBadGenerate . Generate.repl root details ansi artifacts) (toPrintName output)

    case result of
      Left exit ->
        do
          Exit.toStderr (Exit.replToReport exit)
          return oldState
      Right Nothing ->
        return newState
      Right (Just javascript) ->
        do
          exitCode <- interpret interpreter javascript
          case exitCode of
            Exit.ExitSuccess -> return newState
            Exit.ExitFailure _ -> return oldState

interpret :: FilePath -> B.Builder -> IO Exit.ExitCode
interpret interpreter javascript =
  let createProcess = (Proc.proc interpreter []) {Proc.std_in = Proc.CreatePipe}
   in Proc.withCreateProcess createProcess $ \maybeStdIn _ _ handle ->
        do
          let stdin = Maybe.fromJust maybeStdIn
          B.hPutBuilder stdin javascript
          IO.hClose stdin
          Proc.waitForProcess handle

-- TO BYTESTRING

toByteString :: State -> Output -> BS.ByteString
toByteString (State imports types decls) output =
  LBS.toStrict $
    B.toLazyByteString $
      mconcat
        [ "module ",
          N.toBuilder N.replModule,
          " exposing (..)\n",
          Map.foldr mappend mempty imports,
          Map.foldr mappend mempty types,
          Map.foldr mappend mempty decls,
          outputToBuilder output
        ]

outputToBuilder :: Output -> B.Builder
outputToBuilder output =
  N.toBuilder N.replValueToPrint
    <> " ="
    <> case output of
      OutputNothing ->
        " {}\n"
      OutputDecl _ ->
        " {}\n"
      OutputExpr expr ->
        foldr (\line rest -> "\n  " <> B.byteString line <> rest) "\n" (BSC.lines expr)

-- TO PRINT NAME

toPrintName :: Output -> Maybe N.Name
toPrintName output =
  case output of
    OutputNothing -> Nothing
    OutputDecl name -> Just name
    OutputExpr _ -> Just N.replValueToPrint

-- HELP MESSAGES

toHelpMessage :: Maybe String -> String
toHelpMessage maybeBadCommand =
  case maybeBadCommand of
    Nothing ->
      genericHelpMessage
    Just command ->
      "I do not recognize the :" ++ command ++ " command. " ++ genericHelpMessage

genericHelpMessage :: String
genericHelpMessage =
  "Valid commands include:\n\
  \\n\
  \  :exit    Exit the REPL\n\
  \  :help    Show this information\n\
  \  :reset   Clear all previous imports and definitions\n"

-- GET INTERPRETER

getInterpreter :: Maybe String -> IO FilePath
getInterpreter maybeName =
  case maybeName of
    Just name ->
      getInterpreterHelp name (Dir.findExecutable name)
    Nothing ->
      getInterpreterHelp "node` or `nodejs" $
        do
          exe1 <- Dir.findExecutable "node"
          exe2 <- Dir.findExecutable "nodejs"
          return (exe1 <|> exe2)

getInterpreterHelp :: String -> IO (Maybe FilePath) -> IO FilePath
getInterpreterHelp name findExe =
  do
    maybePath <- findExe
    case maybePath of
      Just path ->
        return path
      Nothing ->
        do
          IO.hPutStrLn IO.stderr (exeNotFound name)
          Exit.exitFailure

exeNotFound :: String -> String
exeNotFound name =
  "The REPL relies on node.js to execute JavaScript code outside the browser.\n"
    ++ "I could not find executable `"
    ++ name
    ++ "` on your PATH though!\n\n"
    ++ "You can install node.js from <http://nodejs.org/>. If it is already installed\n"
    ++ "but has a different name, use the --interpreter flag."

-- SETTINGS

initSettings :: IO (Repl.Settings M)
initSettings =
  do
    cache <- Dirs.getReplCache
    return $
      Repl.Settings
        { Repl.historyFile = Just (cache </> "history"),
          Repl.autoAddHistory = True,
          Repl.complete = Repl.completeWord Nothing " \n" lookupCompletions
        }

lookupCompletions :: String -> M [Repl.Completion]
lookupCompletions string =
  do
    (State imports types decls) <- State.get
    return $
      addMatches string False decls $
        addMatches string False types $
          addMatches string True imports $
            addMatches string False commands []

commands :: Map.Map N.Name ()
commands =
  Map.fromList
    [ (":exit", ()),
      (":quit", ()),
      (":reset", ()),
      (":help", ())
    ]

addMatches :: String -> Bool -> Map.Map N.Name v -> [Repl.Completion] -> [Repl.Completion]
addMatches string isFinished dict completions =
  Map.foldrWithKey (addMatch string isFinished) completions dict

addMatch :: String -> Bool -> N.Name -> v -> [Repl.Completion] -> [Repl.Completion]
addMatch string isFinished name _ completions =
  let suggestion = N.toChars name
   in if List.isPrefixOf string suggestion
        then Repl.Completion suggestion suggestion isFinished : completions
        else completions
