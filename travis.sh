#!/bin/bash

set -e errexit

rm -rf hspec-example && git clone https://github.com/hspec/hspec-example

(cd hspec-core     && cabal install --only-dependencies --enable-tests && cabal configure --enable-tests --ghc-options=-Werror && cabal build && cabal test && cabal sdist && cabal install dist/hspec-*.tar.gz)
(cd hspec-discover && cabal install --only-dependencies --enable-tests && cabal configure --enable-tests --ghc-options=-Werror && cabal build && cabal test && cabal sdist && cabal install dist/hspec-*.tar.gz)
(                     cabal install --only-dependencies --enable-tests && cabal configure --enable-tests --ghc-options=-Werror && cabal build && cabal test && cabal sdist && cabal install dist/hspec-*.tar.gz)
(cd hspec-example                                                      && cabal configure --enable-tests --ghc-options=-Werror && cabal build && cabal test)
(cd hspec-th                                                           && cabal configure --enable-tests --ghc-options=-Werror && cabal build && cabal test && cabal sdist && cabal install dist/hspec-*.tar.gz)
(cd hspec-contrib  && cabal install --only-dependencies --enable-tests && cabal configure --enable-tests --ghc-options=-Werror && cabal build && cabal test && cabal sdist && cabal install dist/hspec-*.tar.gz)
(cd hspec-discover/example                                             && cabal configure --enable-tests --ghc-options=-Werror && cabal build && cabal test)
(cd hspec-discover/integration-test                                    && cabal configure --enable-tests --ghc-options=-Werror && cabal build && cabal test)
test/regression/issue-169/run.sh

echo SUCCESS
