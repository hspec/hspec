#!/bin/bash
set -o errexit
set -o nounset

cabal clean

# disable tests that require hspec on the path
sed -i 's/^test-suite hspec-discover-example$/\0\n  buildable: False/' hspec.cabal
sed -i 's/^test-suite hspec-discover-integration-test-empty$/\0\n  buildable: False/' hspec.cabal
sed -i 's/^test-suite hspec-discover-integration-test-with-formatter$/\0\n  buildable: False/' hspec.cabal
sed -i 's/^test-suite hspec-discover-integration-test-with-formatter-empty$/\0\n  buildable: False/' hspec.cabal

cabal sdist

# enable again
sed -i '/^  buildable: False$/d' hspec.cabal

cd dist/
tar xvf hspec-1.*.tar.gz
cd hspec-1.*/
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests --disable-optimization --disable-library-profiling
cabal build && cabal test
