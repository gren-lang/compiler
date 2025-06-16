{-# LANGUAGE OverloadedStrings #-}

module Reporting.Doc
  ( P.Doc,
    (P.<+>),
    (<>),
    P.align,
    P.cat,
    P.empty,
    P.fill,
    P.fillSep,
    P.hang,
    P.hcat,
    P.hsep,
    P.indent,
    P.sep,
    P.vcat,
    P.red,
    P.cyan,
    P.magenta,
    P.green,
    P.blue,
    P.black,
    P.yellow,
    P.dullred,
    P.dullcyan,
    P.dullyellow,
    P.dullwhite,
    --
    fromChars,
    fromName,
    fromVersion,
    fromPackage,
    fromInt,
    --
    toAnsi,
    toString,
    toLine,
    --
    encode,
    --
    stack,
    reflow,
    commaSep,
    --
    toSimpleNote,
    toFancyNote,
    toSimpleHint,
    toFancyHint,
    --
    link,
    fancyLink,
    reflowLink,
    makeLink,
    makeNakedLink,
    --
    args,
    moreArgs,
    ordinal,
    intToOrdinal,
    cycle,
  )
where

import Data.Index qualified as Index
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Name qualified as Name
import Gren.Package qualified as Pkg
import Gren.Version qualified as V
import Json.Encode ((==>))
import Json.Encode qualified as E
import Json.String qualified as Json
import System.Environment qualified
import System.IO (Handle)
import System.Info qualified as Info
import Text.PrettyPrint.ANSI.Leijen qualified as P
import Prettyprinter qualified as PP
import Prelude hiding (cycle)

-- FROM

fromChars :: String -> P.Doc
fromChars =
  P.text

fromName :: Name.Name -> P.Doc
fromName name =
  P.text (Name.toChars name)

fromVersion :: V.Version -> P.Doc
fromVersion vsn =
  P.text (V.toChars vsn)

fromPackage :: Pkg.Name -> P.Doc
fromPackage pkg =
  P.text (Pkg.toChars pkg)

fromInt :: Int -> P.Doc
fromInt n =
  P.text (show n)

-- TO STRING

toAnsi :: Handle -> P.Doc -> IO ()
toAnsi handle doc = do
  docOutput <- maybeRemoveDocColors doc
  P.displayIO handle (P.renderPretty 1 80 docOutput)

maybeRemoveDocColors :: P.Doc -> IO P.Doc
maybeRemoveDocColors doc = do
  noColorEnv <- System.Environment.lookupEnv "NO_COLOR"
  pure $ case noColorEnv of
    Just noColor | noColor /= "" -> P.plain doc
    _ -> doc

toString :: P.Doc -> String
toString doc =
  P.displayS (P.renderPretty 1 80 (P.plain doc)) ""

toLine :: P.Doc -> String
toLine doc =
  P.displayS (P.renderPretty 1 (div maxBound 2) (P.plain doc)) ""

-- FORMATTING

stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat (List.intersperse "" docs)

reflow :: String -> P.Doc
reflow paragraph =
  P.fillSep (map P.text (words paragraph))

commaSep :: P.Doc -> (P.Doc -> P.Doc) -> [P.Doc] -> [P.Doc]
commaSep conjunction addStyle names =
  case names of
    [name] ->
      [addStyle name]
    [name1, name2] ->
      [addStyle name1, conjunction, addStyle name2]
    _ ->
      map (\name -> addStyle name <> ",") (init names)
        ++ [ conjunction,
             addStyle (last names)
           ]

-- NOTES

toSimpleNote :: String -> P.Doc
toSimpleNote message =
  toFancyNote (map P.text (words message))

toFancyNote :: [P.Doc] -> P.Doc
toFancyNote chunks =
  P.fillSep (P.underline "Note" <> ":" : chunks)

-- HINTS

toSimpleHint :: String -> P.Doc
toSimpleHint message =
  toFancyHint (map P.text (words message))

toFancyHint :: [P.Doc] -> P.Doc
toFancyHint chunks =
  P.fillSep (P.underline "Hint" <> ":" : chunks)

-- LINKS

link :: String -> String -> String -> String -> P.Doc
link word before fileName after =
  P.fillSep $
    (P.underline (P.text word) <> ":")
      : map P.text (words before)
      ++ P.text (makeLink fileName)
      : map P.text (words after)

fancyLink :: String -> [P.Doc] -> String -> [P.Doc] -> P.Doc
fancyLink word before fileName after =
  P.fillSep $
    (P.underline (P.text word) <> ":") : before ++ P.text (makeLink fileName) : after

makeLink :: [Char] -> [Char]
makeLink fileName =
  "<" <> makeNakedLink fileName <> ">"

makeNakedLink :: [Char] -> [Char]
makeNakedLink fileName =
  "https://github.com/gren-lang/compiler/blob/"
    <> V.toChars V.compiler
    <> "/hints/"
    <> fileName
    <> ".md"

reflowLink :: [Char] -> [Char] -> [Char] -> P.Doc
reflowLink before fileName after =
  P.fillSep $
    map P.text (words before)
      ++ P.text (makeLink fileName)
      : map P.text (words after)

-- HELPERS

args :: Int -> String
args n =
  show n <> if n == 1 then " argument" else " arguments"

moreArgs :: Int -> String
moreArgs n =
  show n <> " more" <> if n == 1 then " argument" else " arguments"

ordinal :: Index.ZeroBased -> String
ordinal index =
  intToOrdinal (Index.toHuman index)

intToOrdinal :: Int -> String
intToOrdinal number =
  let remainder10 =
        number `mod` 10

      remainder100 =
        number `mod` 100

      ending
        | remainder100 `elem` [11 .. 13] = "th"
        | remainder10 == 1 = "st"
        | remainder10 == 2 = "nd"
        | remainder10 == 3 = "rd"
        | otherwise = "th"
   in show number <> ending

cycle :: Int -> Name.Name -> [Name.Name] -> P.Doc
cycle indent name names =
  let toLn n = cycleLn <> P.dullyellow (fromName n)
   in P.indent indent $
        P.vcat $
          cycleTop : List.intersperse cycleMid (toLn name : map toLn names) ++ [cycleEnd]

cycleTop, cycleLn, cycleMid, cycleEnd :: P.Doc
cycleTop = if isWindows then "+-----+" else "┌─────┐"
cycleLn = if isWindows then "|    " else "│    "
cycleMid = if isWindows then "|     |" else "│     ↓"
cycleEnd = if isWindows then "+-<---+" else "└─────┘"

isWindows :: Bool
isWindows =
  Info.os == "mingw32"

-- JSON

encode :: P.Doc -> E.Value
encode doc =
  E.array (toJsonHelp noStyle [] (P.renderPretty 1 80 doc))

data Style = Style
  { _bold :: Bool,
    _underline :: Bool,
    _color :: Maybe Color
  }

noStyle :: Style
noStyle =
  Style False False Nothing

data Color
  = Red
  | RED
  | Magenta
  | MAGENTA
  | Yellow
  | YELLOW
  | Green
  | GREEN
  | Cyan
  | CYAN
  | Blue
  | BLUE
  | Black
  | BLACK
  | White
  | WHITE

toJsonHelp :: Style -> [Text] -> P.SimpleDoc -> [E.Value]
toJsonHelp style revChunks simpleDoc =
  case simpleDoc of
    PP.SFail ->
      error
        "according to the main implementation, @SFail@ can not\
        \ appear uncaught in a rendered @SimpleDoc@"
    PP.SEmpty ->
      [encodeChunks style revChunks]
    PP.SChar char rest ->
      toJsonHelp style (Text.singleton char : revChunks) rest
    PP.SText _ string rest ->
      toJsonHelp style (string : revChunks) rest
    PP.SLine indent rest ->
      toJsonHelp style (Text.replicate indent (Text.singleton ' ') : Text.singleton '\n' : revChunks) rest
    PP.SAnnPush _ rest ->
      toJsonHelp style revChunks rest
    PP.SAnnPop rest ->
      toJsonHelp style revChunks rest


encodeChunks :: Style -> [Text] -> E.Value
encodeChunks (Style bold underline color) revChunks =
  let chars = Text.unpack $ Text.concat (reverse revChunks)
   in case color of
        Nothing
          | not bold && not underline ->
              E.chars chars
        _ ->
          E.object
            [ "bold" ==> E.bool bold,
              "underline" ==> E.bool underline,
              "color" ==> maybe E.null encodeColor color,
              "string" ==> E.chars chars
            ]

encodeColor :: Color -> E.Value
encodeColor color =
  E.string $
    Json.fromChars $
      case color of
        Red -> "red"
        RED -> "RED"
        Magenta -> "magenta"
        MAGENTA -> "MAGENTA"
        Yellow -> "yellow"
        YELLOW -> "YELLOW"
        Green -> "green"
        GREEN -> "GREEN"
        Cyan -> "cyan"
        CYAN -> "CYAN"
        Blue -> "blue"
        BLUE -> "BLUE"
        Black -> "black"
        BLACK -> "BLACK"
        White -> "white"
        WHITE -> "WHITE"
