#!/bin/bash
git submodule update --init --recursive
echo -e "packages:\t.\n\t\t\t.." > Haskell-SFML/demos/cabal.project