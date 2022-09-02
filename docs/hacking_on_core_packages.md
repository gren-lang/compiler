# Hacking on core packages

When making changes to core packages that contain kernel code, a bit of hackery is required due to the constraints enforced by the compiler.

This document describes making changes to a core package used in the context of an application. The intent of the application is to be able to test the results of the changes made to the package.

## Package resolution

When compiling a project, the Gren compiler reads the list of dependencies from `gren.json` and checks to see if they are all downloaded to disk. This check simply sees if a specific `gren.json` file exists at a specific file path. If the file path exists, then the compiler will treat whatever source code is there as the correct code for the package and version in question.

For instance, if your project has a dependency on `gren/core 1.0.0`, then the Gren compiler will look for a `gren.json` file in `~/.cache/gren/<compiler_version>/packages/gren/core/1.0.0`. The `~/.cache/gren/` folder is otherwise known as the package cache.

This means that if you want to test a bug-fix for `gren/core`, you can simply edit the source code at that location then compile your application and see if the bug is fixed.

The compiler follows symlinks, so you could replace a package directory with a symlink to your local checkout of that package.

## Caches

In order to speed up development, Gren caches the compiled result of packages to avoid doing it again in the future.

When making changes to a package that resides in the package cache, as described above, you'll also need to remove these compilation caches for the changes to be included in the next build.

In the package cache, you'll need to delete a `artifacts.dat` file in the directory of a versioned package.

In your application, you'll need to remove the `.gren` directory.

Once those are removed, running `gren make` will recompile both the package and the application, and all recent changes should be present.
