{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.List qualified as List
import Docs qualified
import Format qualified
import Gren.Platform qualified as Platform
import Gren.Version qualified as V
import Init qualified
import Make qualified
import Package qualified
import Repl qualified
import Terminal
import Terminal.Helpers qualified as H
import Text.PrettyPrint.ANSI.Leijen qualified as P
import Prelude hiding (init)

-- MAIN

main :: IO ()
main =
  Terminal.app
    intro
    outro
    [ repl,
      init,
      make,
      docs,
      format,
      package
    ]

intro :: P.Doc
intro =
  P.vcat
    [ P.fillSep
        [ "Hi,",
          "thank",
          "you",
          "for",
          "trying",
          "out",
          P.green "Gren",
          P.green (P.text (V.toChars V.compiler)) <> ".",
          "I hope you like it!"
        ],
      "",
      P.black "-------------------------------------------------------------------------------",
      P.black "I highly recommend working through <https://gren-lang.org/learn> to get started.",
      P.black "It teaches many important concepts, including how to use `gren` in the terminal.",
      P.black "-------------------------------------------------------------------------------"
    ]

outro :: P.Doc
outro =
  P.fillSep $
    map P.text $
      words
        "Be sure to ask on the Gren Discord (https://discord.gg/J8aaGMfz) if you run into trouble!\
        Folks are friendly and happy to help out. They hang out there because it is fun, so be kind\
        to get the best results!"

-- INIT

init :: Terminal.Command
init =
  let summary =
        "Start an Gren project. It creates a starter gren.json file."

      details =
        "The `init` command helps start Gren projects:"

      example =
        reflow
          "It will ask permission to create an gren.json file, the one thing common\
          \ to all Gren projects."

      initFlags =
        flags Init.Flags
          |-- onOff "yes" "Assume yes for all interactive prompts."
          |-- onOff "package" "Create a package (as opposed to an application)."
          |-- flag "platform" initPlatformParser "Which platform to target"
   in Terminal.Command "init" (Common summary) details example noArgs initFlags Init.run

initPlatformParser :: Parser Platform.Platform
initPlatformParser =
  Parser
    { _singular = "platform",
      _plural = "platforms",
      _parser = Platform.fromChars,
      _suggest = \_ -> return ["common", "browser", "node"],
      _examples = \_ -> return ["common", "browser", "node"]
    }

-- REPL

repl :: Terminal.Command
repl =
  let summary =
        "Open up an interactive programming session. Type in Gren expressions\
        \ like (2 + 2) or (String.length \"test\") and see if they equal four!"

      details =
        "The `repl` command opens up an interactive programming session:"

      example =
        reflow
          "Start working through <https://gren-lang.org/learn> to learn how to use this!\
          \ It has a whole chapter that uses the REPL for everything, so that is probably\
          \ the quickest way to get started."

      replFlags =
        flags Repl.Flags
          |-- flag "interpreter" interpreter "Path to a alternate JS interpreter, like node or nodejs."
          |-- onOff "no-colors" "Turn off the colors in the REPL. This can help if you are having trouble reading the values. Some terminals use a custom color scheme that diverges significantly from the standard ANSI colors, so another path may be to pick a more standard color scheme."
   in Terminal.Command "repl" (Common summary) details example noArgs replFlags Repl.run

interpreter :: Parser String
interpreter =
  Parser
    { _singular = "interpreter",
      _plural = "interpreters",
      _parser = Just,
      _suggest = \_ -> return [],
      _examples = \_ -> return ["node", "nodejs"]
    }

-- MAKE

make :: Terminal.Command
make =
  let details =
        "The `make` command compiles Gren code into JS or HTML:"

      example =
        stack
          [ reflow
              "For example:",
            P.indent 4 $ P.green "gren make src/Main.gren",
            reflow
              "This tries to compile an Gren file named src/Main.gren, generating an index.html\
              \ file if possible."
          ]

      makeFlags =
        flags Make.Flags
          |-- onOff "debug" "Turn on the time-travelling debugger. It allows you to rewind and replay events. The events can be imported/exported into a file, which makes for very precise bug reports!"
          |-- onOff "optimize" "Turn on optimizations to make code smaller and faster. For example, the compiler renames record fields to be as short as possible and unboxes values to reduce allocation."
          |-- onOff "sourcemaps" "Add sourcemaps to the resulting JS file. This let's you debug Gren code in a JS debugger, at the cost of longer compile times and a bigger JS file."
          |-- flag "output" Make.output "Specify the name of the resulting JS file. For example --output=assets/gren.js to generate the JS at assets/gren.js. You can also use --output=/dev/stdout to output the JS to the terminal, or --output=/dev/null to generate no output at all!"
          |-- flag "report" Make.reportType "You can say --report=json to get error messages as JSON. This is only really useful if you are an editor plugin. Humans should avoid it!"
   in Terminal.Command "make" Uncommon details example (zeroOrMore H.grenFile) makeFlags Make.run

-- DOCS

docs :: Terminal.Command
docs =
  let details =
        "The `docs` command collects all documentation for a package in a JSON file:"

      example =
        stack
          [ reflow
              "For example:",
            P.indent 4 $ P.green "gren docs",
            reflow
              "This collects all documentation for the current package and writes it to a\
              \ docs.json file, if possible"
          ]

      docsFlags =
        flags Docs.Flags
          |-- flag "output" Docs.output "Specify the name of the resulting JSON file. For example --output=assets/docs.json to generate the JSON at assets/docs.json. You can also use --output=/dev/stdout to output the JSON to the terminal, or --output=/dev/null to verify that generating the documentation would work."
          |-- flag "report" Docs.reportType "You can say --report=json to get error messages as JSON. This is only really useful if you are an editor plugin. Humans should avoid it!"
   in Terminal.Command "docs" Uncommon details example noArgs docsFlags Docs.run

-- PACKAGE

package :: Terminal.Command
package =
  let details =
        "The `package` command contains sub-commands which help you manage your package,\
        \ or your dependant packages:"

      example =
        stack
          [ reflow
              "For example, if you want to get access to Web APIs in your project,\
              \ you would say:",
            P.indent 4 $
              P.green $
                P.vcat
                  [ "gren package install gren-lang/browser"
                  ],
            reflow
              "To see a description of all available sub-commands, execute:",
            P.indent 4 $
              P.green $
                P.vcat
                  ["gren package --help"]
          ]
   in Terminal.Prefix "package" details example Package.run

-- FORMAT

format :: Terminal.Command
format =
  let details =
        "The `format` command rewrites .gren files to use Gren's preferred style:"

      example =
        reflow "If no files or directories are given, all .gren files in all source and test directories will be formatted."

      formatFlags =
        flags Format.Flags
          |-- onOff "yes" "Assume yes for all interactive prompts."
          |-- onOff "stdin" "Format stdin and write it to stdout."
          |-- onOff "validate" "Check if input is correctly formatted."
   in Terminal.Command "format" Uncommon details example (zeroOrMore H.grenFileOrDirectory) formatFlags Format.run

-- HELPERS

stack :: [P.Doc] -> P.Doc
stack docList =
  P.vcat $ List.intersperse "" docList

reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string
