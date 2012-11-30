#!/bin/bash
set -o errexit

if [ -z $TRAVIS ]; then
  TRAVIS=false
fi

# do not require hspec-discover on PATH
if ! $TRAVIS; then
  hspec-discover test/Spec.hs test/Spec.hs test/Spec.hs
  hspec-discover hspec-discover/test/Spec.hs hspec-discover/test/Spec.hs hspec-discover/test/Spec.hs
fi

# package an run tests
cabal clean
cabal sdist
cd dist/
tar xvf hspec-1.*.tar.gz
cd hspec-1.*/

if $TRAVIS; then
  cabal install --only-dependencies --enable-tests
fi

cabal configure --enable-tests --disable-optimization --disable-library-profiling
cabal build && cabal test
