# Creating an Gren project

The main goal of `gren init` is to get you to this page!

It just creates an `gren.json` file and a `src/` directory for your code.


## What is `gren.json`?

This file describes your project. It lists all of the packages you depend upon, so it will say the particular version of [`gren/core`](https://package.gren-lang.org/packages/gren/core/latest/) and [`gren/html`](https://package.gren-lang.org/packages/gren/html/latest/) that you are using. It makes builds reproducible! You can read a bit more about it [here](https://github.com/gren/compiler/blob/master/docs/gren.json/application.md).

You should generally not edit it by hand. It is better to add new dependencies with commands like `gren install gren/http` or `gren install gren/json`.


## What goes in `src/`?

This is where all of your Gren files live. It is best to start with a file called `src/Main.gren`. As you work through [the official guide](https://guide.gren-lang.org/), you can put the code examples in that `src/Main.gren` file.


## How do I compile it?

You can run `gren make src/Main.gren` and it will produce an `index.html` file that you can look at in your browser.
