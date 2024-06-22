#!/usr/bin/env bash
set -e

cabal build -f dev
cp `cabal list-bin .` .
