# Local development commands

- build: `cabal build -f dev`
- run tests: `cabal test -f dev`
- format files: `ormolu --mode inplace $(git ls-files '*.hs')`
