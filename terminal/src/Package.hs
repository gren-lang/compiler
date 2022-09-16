{-# LANGUAGE OverloadedStrings #-}

module Package
  ( run,
  )
where

import Data.List qualified as List
import Package.Bump qualified as Bump
import Package.Diff qualified as Diff
import Package.Install qualified as Install
import Package.Uninstall qualified as Uninstall
import Package.Validate qualified as Validate
import Terminal
import Terminal.Helpers
import Text.PrettyPrint.ANSI.Leijen qualified as P

-- RUN

run :: [String] -> IO ()
run =
  Terminal.prefix
    intro
    P.empty
    [ install,
      uninstall,
      bump,
      diff,
      validate
    ]

intro :: P.Doc
intro =
  reflow
    "This is a group of commands for helping you manage packages,\
    \ your own or those you depend on."

-- INSTALL

install :: Terminal.Command
install =
  let details =
        "The `install` command fetches packages from github for\
        \ use in your project:"

      summary =
        "Add dependencies to your project."

      example =
        stack
          [ reflow
              "For example, if you want to get access to Web APIs in your project, you would say:",
            P.indent 4 $
              P.green $
                P.vcat $
                  [ "gren package install gren-lang/browser"
                  ],
            reflow
              "Notice that you must say the AUTHOR name and PROJECT name! After running those\
              \ commands, you could say `import Browser` in your code.",
            reflow
              "What if two projects use different versions of the same package? No problem!\
              \ Each project is independent, so there cannot be conflicts like that!"
          ]

      installArgs =
        oneOf
          [ require0 Install.NoArgs,
            require1 Install.Install package
          ]

      installFlags =
        flags Install.Flags
          |-- onOff "yes" "Assume yes for all interactive prompts."
   in Terminal.Command "install" (Common summary) details example installArgs installFlags Install.run

-- UNINSTALL

uninstall :: Terminal.Command
uninstall =
  let details =
        "The `uninstall` command removes packages from your project:"

      example =
        stack
          [ reflow
              "For example, if you want to get rid of the browser package in your project,\
              \ you would say:",
            P.indent 4 $
              P.green $
                P.vcat $
                  [ "gren package uninstall gren-lang/browser"
                  ],
            reflow
              "Notice that you must say the AUTHOR name and PROJECT name!",
            reflow
              "What if another package depends on what you're trying to remove? No problem!\
              \ I'll let you know if that's the case, and help you resolve that situation."
          ]

      uninstallArgs =
        require1 Uninstall.Uninstall package

      uninstallFlags =
        flags Uninstall.Flags
          |-- onOff "yes" "Assume yes for all interactive prompts."
   in Terminal.Command "uninstall" Uncommon details example uninstallArgs uninstallFlags Uninstall.run

-- VALIDATE

validate :: Terminal.Command
validate =
  let details =
        "The `validate` command checks that you have done everything necessary\
        \ so that anyone in the Gren community can use your package."

      example =
        stack
          [ reflow
              "Gren packages are \"just\" git repositories hosted on github. As\
              \ long as you've tagged your repository with semver formatted tags,\
              \ anyone can add your package as a dependency.",
            reflow
              "However, a package is no better than its documentation. This command\
              \ therefore checks that your package exposes a bare minimum of\
              \ prose that the users of your package can read.",
            reflow
              "Keep in mind, you don't just want to tell people HOW to use\
              \ your package. It's equally important to tell them WHY. What\
              \ problem does it solve? Why should people use this particular\
              \ package to solve their problem?",
            reflow
              "Once this command passes, you may want to add your repo to\
              \ https://packages.gren-lang.org so it becomes easier for the Gren\
              \ community to find your package, and its documentation."
          ]
   in Terminal.Command "validate" Uncommon details example noArgs noFlags Validate.run

-- BUMP

bump :: Terminal.Command
bump =
  let details =
        "The `bump` command figures out the next version number based on API changes:"

      example =
        reflow
          "Say you just published version 1.0.0, but then decided to remove a function.\
          \ I will compare the published API to what you have locally, figure out that\
          \ it is a MAJOR change, and bump your version number to 2.0.0. I do this with\
          \ all packages, so there cannot be MAJOR changes hiding in PATCH releases in Gren!"
   in Terminal.Command "bump" Uncommon details example noArgs noFlags Bump.run

-- DIFF

diff :: Terminal.Command
diff =
  let details =
        "The `diff` command detects API changes:"

      example =
        stack
          [ reflow
              "For example, to see what changed in the Browser package between\
              \ versions 1.0.0 and 2.0.0, you can say:",
            P.indent 4 $ P.green $ "gren package diff gren-lang/browser 1.0.0 2.0.0",
            reflow
              "Sometimes a MAJOR change is not actually very big, so\
              \ this can help you plan your upgrade timelines."
          ]

      diffArgs =
        oneOf
          [ require0 Diff.CodeVsLatest,
            require1 Diff.CodeVsExactly version,
            require2 Diff.LocalInquiry version version,
            require3 Diff.GlobalInquiry package version version
          ]
   in Terminal.Command "diff" Uncommon details example diffArgs noFlags Diff.run

-- HELPERS

stack :: [P.Doc] -> P.Doc
stack docList =
  P.vcat $ List.intersperse "" docList

reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string
