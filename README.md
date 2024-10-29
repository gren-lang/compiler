# Gren

Compiler for Gren, a pure functional programming language that is easy to learn, but powerful in use.

There are easier ways to install the compiler than compiling the source, you might want to read the [setup instructions](https://gren-lang.org/install).

## Build from source

This project uses [devbox](https://www.jetify.com/devbox) for managing dependencies required to build the project. If you don't want to use devbox,
you can find a list of the requried dependencies and the commands for building the compiler in `devbox.json`.

Since Gren 0.4 the compiler is implemented in two parts. The Gren-part of the compiler lives in `src`, and once built it acts
as a frontend to the Haskell-part of the compiler.

The end goal is for the entire compiler to be written in Gren.

To build the compiler:

1. Use `devbox run prepare-deps` to setup the required dependencies. The first time you run this it might take a while.
2. Build the compiler with `devbox run build`.

You can now execute the compiler with `node ./cli.js` or just `./cli.js`. The Gren-part of the compiler will download a pre-built
binary of the Haskell-part from Github. If you want to run the compiler with the Haskell-part you've built yourself, set the path
to the Haskell-binary in a `GREN_BIN` environment variable.

Don't forget to check the `scripts` defined in `devbox.json`. They can be executed using `devbox run <script>`.
