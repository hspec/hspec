# hspec: Behavior Driven Development for Haskell

## Getting started

Install hspec from Hackage.

    cabal update && cabal install hspec

Read the [introductory documentation](http://hspec.github.com/).

## Development & Support

Join in at `#hspec` on freenode.

### Running Hspec's own test suite

Hspec is tested with `hspec-meta`.  You need to install `hspec-meta` before you
can run the test suite:

    git checkout hspec-meta && caba install && git checkout master

To run the test suite do:

    cabal configure --enable-tests && cabal build && cabal test

## Contributors

 * Trystan Spangler
 * Greg Weber
 * Björn Buckwalter
 * Yi Huang
 * Lucas Severyn
 * Simon Hengel
 * Niklas Hambüchen
 * Clark Gaebel
