# Creating a Gren project

The main goal of `gren init` is to get you to this page!

It just creates an `gren.json` file and a `src/` directory for your code.

## What is `gren.json`?

This file describes your project. It lists all of the packages you depend upon, so it will say the particular version of [`gren-lang/core`](https://packages.gren-lang.org/package/gren-lang/core/latest/overview) and [`gren-lang/browser`](https://packages.gren-lang.org/package/gren-lang/browser/latest/overview) that you are using. It makes builds reproducible! You can read a bit more about it [here](https://gren-lang.org/book/appendix/gren_json/).

You should generally not edit it by hand. It is better to add new dependencies with commands like `gren package install gren-lang/test` or `gren package install gren-lang/parser`.

## What goes in `src/`?

This is where all of your Gren files live. It is best to start with a file called `src/Main.gren`. As you work through [the official guide](https://gren-lang.org/learn), you can put the code examples in that `src/Main.gren` file.

## How do I compile it?

You can run `gren make src/Main.gren` and it will produce an `index.html` file that you can look at in your browser.
