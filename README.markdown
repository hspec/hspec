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
 - Rebase `travis-ci-osx` on `HEAD` and make sure that the build passes
 - Release new version of `hspec-meta`
 - Update version constraint for `hspec-meta`
 - Add `find hspec-discover/test-data/ -type f` under `extra-source-files` in
   `hspec.cabal`
 - Release new version of `hspec`
 - Upload docs

## Contributors

 * Alan Zimmerman
 * Alfredo Di Napoli
 * Björn Buckwalter
 * Clark Gaebel
 * Daniel Gröber
 * Fujimura Daisuke
 * Greg Weber
 * Harry Garrood
 * Jan Matějka
 * Jeffrey Pigarelli
 * Julian K. Arni
 * Junji Hashimoto
 * Konstantine Rybnikov
 * Lucas Severyn
 * Markus Klinik
 * Mateusz Kowalczyk
 * Matvey B. Aksenov
 * Michael Snoyman
 * Niklas Hambüchen
 * Ömer Sinan Ağacan
 * Pedro Tacla Yamada
 * Peter Simons
 * Raine Virta
 * Ron Watkins
 * Ryan Mulligan
 * Simon Hengel
 * Sönke Hahn
 * Takayuki Muranushi
 * Trystan Spangler
 * Yi Huang
 * Zhang Yichao
