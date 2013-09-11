# Hspec: Behavior-Driven Development for Haskell

## Getting started

Install Hspec from Hackage.

    cabal update && cabal install hspec

Read the [User's Manual](http://hspec.github.io/).

## Development & Support

Join in at `#hspec` on freenode.

### Running Hspec's own test suite

Hspec is tested with `hspec-meta`.  You need to install `hspec-meta` before you
can run the test suite:

    cabal install --enable-tests --only-dependencies

To run the test suite do:

    cabal configure --enable-tests && cabal build && cabal test

### Preparing a release

 - Bump version
 - Add `find hspec-discover/test-data/ -type f` under `extra-source-files` in
   `hspec.cabal`
 - Make sure that `other-modules` for `test-suite spec` is up-to-date
 - Use `./mk-sdist.sh` to create and test the tarball
 - Release an updated version of `hspec-meta`

## Contributors

 * Trystan Spangler
 * Greg Weber
 * Björn Buckwalter
 * Yi Huang
 * Lucas Severyn
 * Simon Hengel
 * Niklas Hambüchen
 * Clark Gaebel
 * Fujimura Daisuke
 * Michael Snoyman
 * Takayuki Muranushi
 * Sönke Hahn
 * Peter Simons
 * Markus Klinik
 * Konstantine Rybnikov
 * Alfredo Di Napoli
 * Matvey B. Aksenov
