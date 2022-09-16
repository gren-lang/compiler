# Gren

Compiler for the Gren, a pure functional programming language that is easy to learn, but powerful in use.

There are easier ways to install the compiler than compiling the source, you might want to read the [setup instructions](https://gren-lang.org/install).

## Build from source

Then Gren compiler is written in Haskell, so to build from source you need to have GHC 9.4 (Haskell compiler) and Cabal 3.8 (haskell build tool) installed on your system.

You can install these using [ghcup](https://www.haskell.org/ghcup/). By default, ghcup will install an older version of Haskell and Cabal, so you can install and set the required versions using `ghcup tui`.

Compiling and installing the project should just be a matter of `cabal install`, after which you should be able to run the `gren` command from your command line.

Read the `CONTRIBUTING.md` file for some helpful commands for working on the compiler itself.
