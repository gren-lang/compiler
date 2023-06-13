# Gren Roadmap

Here is a plan for what future releases of Gren will focus on, on the road to 1.0.

This is to be considered a living document. Releases may arrive late due to real-life changes of the contributors involved. We may also learn new things along the way which change the things we wish to focus on.

Also, keep in mind that this only focuses on the big picture. Major features like parametric modules and Web Assembly are likely to cause an avalanche of other changes as well. 

## Releases

* December 2023 - Parametric modules, or OCaml functors, to enable defining things like equality, comparisons etc. for your own types and use them in data structures like Dict. This release also removes the "magic" type classes currently in the language, like `number` and `comparable`.

* June 2024 - Stabelization period. Mostly bugfixes and code gen improvements. While we prepare for the next big thing.

* December 2024 - Re-evaluating how concurrency works in the language. More specifically, we'll re-evaluate how `Task`, `Cmd`, `Process` and effect modules work.

* June 2025 - Re-evaluate records and custom types.

* December 2025 - Re-evaluate interop: ports and kernel code.

* June 2026 - Compile to Web Assembly instead of JS. This will give us proper integers, big integers, hopefully smaller asset size and possibly enable future optimizations in codegen.

* December 2026 - The language is considered complete. Both the compiler and core packages will commit to maintaining backwards compatability from here on out.
