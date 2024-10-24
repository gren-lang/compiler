# Gren

Compiler for Gren, a pure functional programming language that is easy to learn, but powerful in use.

There are easier ways to install the compiler than compiling the source, you might want to read the [setup instructions](https://gren-lang.org/install).

## Build from source

This project uses [devbox](https://www.jetify.com/devbox) for managing dependencies required to build the project. If you don't want to use devbox,
you can find a list of the requried dependencies and their specific versions in `devbox.json`.

Since Gren 0.4 the compiler is implemented in two parts. The Gren-portion of the compiler lives in `src`, and once built it acts
as a frontend to the Haskell-portion of the compiler.

The end goal is for the entire compiler to be written in Gren, but for now you need to build both Gren- and Haskell-portions of the compiler.

1. Use `devbox shell` to get a terminal with the required dependencies installed.
2. Use `./build_dev_bin.sh` to build the Haskell-based compiler. You should now have a `./gren` file in your directory.
3. Build the Gren compiler with `npm run prepublishOnly`
4. You can now execute your local build with `GREN_BIN=./gren node ./cli.js`

`GREN_BIN` tells the compiler which Haskell-binary to communicate with. If not specified, it will be downloaded.
