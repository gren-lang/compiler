{-# LANGUAGE OverloadedStrings #-}

module Package
  ( run,
  )
where

import Bump qualified
import Data.List qualified as List
import Diff qualified
import Install qualified
import Publish qualified
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
      bump,
      diff,
      publish
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
   in Terminal.Command "install" (Common summary) details example installArgs noFlags Install.run

-- PUBLISH

publish :: Terminal.Command
publish =
  let details =
        "The `publish` command publishes your package on <https://package.gren-lang.org>\
        \ so that anyone in the Gren community can use it."

      example =
        stack
          [ reflow
              "Think hard if you are ready to publish NEW packages though!",
            reflow
              "Part of what makes Gren great is the packages ecosystem. The fact that\
              \ there is usually one option (usually very well done) makes it way\
              \ easier to pick packages and become productive. So having a million\
              \ packages would be a failure in Gren. We do not need twenty of\
              \ everything, all coded in a single weekend.",
            reflow
              "So as community members gain wisdom through experience, we want\
              \ them to share that through thoughtful API design and excellent\
              \ documentation. It is more about sharing ideas and insights than\
              \ just sharing code! The first step may be asking for advice from\
              \ people you respect, or in community forums. The second step may\
              \ be using it at work to see if it is as nice as you think. Maybe\
              \ it ends up as an experiment on GitHub only. Point is, try to be\
              \ respectful of the community and package ecosystem!",
            reflow
              "Check out <https://package.gren-lang.org/help/design-guidelines> for guidance on how to create great packages!"
          ]
   in Terminal.Command "publish" Uncommon details example noArgs noFlags Publish.run

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
              "For example, to see what changed in the HTML package between\
              \ versions 1.0.0 and 2.0.0, you can say:",
            P.indent 4 $ P.green $ "gren diff gren/html 1.0.0 2.0.0",
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
