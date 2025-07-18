#!/usr/bin/env bash

set -e

npx --package=gren-lang@0.5 gren make src/Main.gren --optimize --output=app
terser app -c -m -o bin/compiler

# Check to see that we haven't changed the state of the repo
# If this fails, a new commit should be made prior to publishing
test -z "$(git status --porcelain)" || (echo "production build of compiler is not commited!" && exit 1)
