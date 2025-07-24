# Kernel code

Kernel code is JavaScript which can be called directly from Elm. As in, it is not compiled from Elm source code. Such code is inherently unsafe, but is necessary for reasons of performance and for exposing Web APIs to Gren.

Because it is unsafe, kernel code has certain limitations. Chief among them is that it can only be contained within a `gren/` package. As such, kernel code can only be written through collaboration with the Gren core team.

If you have an idea for a package that needs to make use of kernel code, please reach out to the core team to discuss this idea before venturing forth.

## How do I write kernel code?

As mentioned, kernel code has certain limitations:

- Only packages can contain kernel code, not applications.
- The package _has to_ be hosted in the `gren` organization on GitHub.
- Kernel code must be placed in JavaScript file in the `src/Gren/Kernel` directory of the package.
- Kernel code files can be imported by Gren modules, but cannot be aliased or have an exposing list.
- Kernel code needs to be wrapped by a Gren function _with_ type annotations.

When writing kernel code, you should aim to write as little of it as possible. Kernel code is susceptible to break between compiler releases, and by reducing it to a minimum it becomes easier to upgrade it to a new compiler release.

There are also many rough edges when dealing with kernel code, and smoothing over those edges is not a top priority.

## Structure of a kernel code file

Let's say we want to create a package for accessing the `LocalStorage` API. Create a new package (the package name needs to be something like `gren/local-storage`, and create a `src/Gren/Kernel/LocalStorage.js` file.

The implementation might look like this:

```
/*
import Maybe exposing (Just, Nothing)
*/

function _LocalStorage_getItem(key) {
    var item = localStorage.getItem(key);
    if (typeof item !== 'undefined') {
        return __Maybe_Just(item);
    }

    return __Maybe_Nothing;
}
```

A few things to note:

- Each \*.js file _must_ begin with a multi-line comment. Inside this comment block, you can use Gren-like syntax to import Gren and Kernel code modules.
- When defining functions and variables in the module, it needs to be prefixed with a _single_ underscore and the name of the file. In this case: `_LocalStorage_functionName`.
- When calling a function, or referencing a variable in the same module, it must use the same prefix as when defining something.
- When calling an external function or referencing an external variable, you must use _two_ underscores and the module name. In this case: `__Maybe_Just(item)`.
- Object properties must be prefixed with `__$` on the javascript side if the objects come from gren, or are being passed to gren. Otherwise things will crash when `--optimize` mangles the names. E.g. `Gren.Kernel.MyModule.myFunc { foo = "bar" }` on the js side should access `foo` with `myObj.__$foo`, and vice versa sending an object to gren from js: `__MyModule_anotherFunc({__$bar: "baz"})` would allow gren to access `bar` with `myObject.bar`.
- Defining a function that takes more than two arguments, must be constructed using a curried function helper. For instance, defining a function like `setItem` would look like: `var \_LocalStorage_setItem = F2(function(key, value) { ... });
- Calling a function that takes between 2-9 arguments must be called using a partial application helper. As an example, calling a function with two arguments look like this: `A2(_LocalStorage_setItem, key, value)`.
- Alternatively, you can simply perform a curried function call, though this will be worse for performance: `_LocalStorage_setItem(key)(value)`.

Also keep in mind that the code above has noticeable side effects, and wouldn't pass code review by the core team. In this particular instance, the `getItem` function should probably return a `Task`. You can take a look at the `Process` module in `gren/core` for an example.

## Calling kernel code form Gren

Following the example above, create a Gren file at `src/LocalStorage.gren`. It might look like the following:

```
module LocalStorage exposing (getItem)

import Gren.Kernel.LocalStorage

getItem : String -> Maybe String
getItem =
  Gren.Kernel.LocalStorage.getItem
```

Nothing is being done by the Gren compiler to verify that the kernel code adheres to the type signature, this needs to be verified manually to avoid strange effects in otherwise safe Gren code.
