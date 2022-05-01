
# Local development commands

- build: `cabal build -f dev`
- run tests: `cabal test -f dev`
- format files: `find -name '*.hs' | xargs -t ormolu -m inplace`
