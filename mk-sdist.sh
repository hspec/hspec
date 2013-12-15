#!/bin/bash

set -o errexit
set -o nounset

cabal clean
cabal sdist

cd dist/
tar xvf hspec-1.*.tar.gz
cd hspec-1.*/
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests --disable-optimization --disable-library-profiling
cabal build && cabal test
