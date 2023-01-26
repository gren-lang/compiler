# Gren Roadmap

Here is a plan for what future releases of Gren will focus on, on the road to 1.0.

This is to be considered a living document. Releases may arrive late due to real-life changes of the contributors involved. We may also learn new things along the way which change the things we wish to focus on.

Also, keep in mind that this only focuses on the big picture. Major features like parametric modules and Web Assembly are likely to cause an avalanche of other changes as well. 

## Releases

* June 2023 - Source maps and improved code generation to better support the built-in debugger in Node and browsers.

* December 2023 - Additions and bugfixes to core packages like gren-lang/core, gren-lang/browser and gren-lang/node. The compiler will mostly receive bugfixes in this period.

* June 2024 - Parametric modules, or OCaml functors, to enable defining things like equality, comparisons etc. for your own types and use them in data structures like Dict. This release also removes the "magic" type classes currently in the language, like `number` and `comparable`.

* June 2025 - Compile to Web Assembly instead of JS. This will give us proper integers, big integers, hopefully smaller asset size and possibly enable future optimizations in codegen.

* December 2025, or June 2026 - The language is considered complete. Both the compiler and core packages will commit to maintaining backwards compatability from here on out.
