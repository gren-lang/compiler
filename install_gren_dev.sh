#!/usr/bin/env bash
set -e

cabal build -f dev
cp `cabal list-bin .` ~/.cabal/bin/gren

echo "New gren binary installed"