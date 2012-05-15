# Automatically discover and run Hspec tests

## Getting started

> For the purpose of this example all source files are put into the `src/`
> directory, specs are put into the `test/` directory, and the main module of
> the test suite is named `Spec.hs`.  This are useful conventions, but you can
> use anything you like.

Create some modules.

```
src/Foo.hs
src/Foo/Bar.hs
src/Baz.hs
```

And write some specs.

```
test/FooSpec.hs
test/Foo/BarSpec.hs
test/BazSpec.hs
```
Create a main module for your test suite, containing one single line.

```haskell
-- file test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

Run it (e.g. with `ghci`).

```
$ echo ':set -isrc -itest' > .ghci
$ ghci test/Spec.hs
*Main> main
```

Or use it in a Cabal test suite.

```
test-suite spec
  type:           exitcode-stdio-1.0
  main-is:        Spec.hs
  hs-source-dirs: src, test
  build-depends:  base, hspec, hspec-discover
```

## How it works

`-F -pgmF hspec-discover` tells GHC to preprocess a module with
`hspec-discover`.  `hspec-discover` will then generate a suitable main module
for your test suite.

For the above example something like this is generated.

```haskell
module Main where

import Test.Hspec.Monadic

import qualified BazSpec
import qualified FooSpec
import qualified Foo.BarSpec

main :: IO ()
main = hspecX $ do
  describe "Foo"     FooSpec.spec
  describe "Foo.Bar" Foo.BarSpec.spec
  describe "Baz"     BazSpec.spec
```

`hspec-discover` searches for specs in the same directory where the file with
the preprocessor directive is located, and in all subdirectories.  All files
with a name that ends in `Spec.hs` are include in the generated test suite.
And it is assumed, that they export a `spec` of type
`Test.Hspec.Monadic.Specs`.

## Customizing things

If you rather want to get a nested spec for hierarchical modules, pass
`--nested` to `hspec-discover`.

```haskell
-- file Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --nested #-}
```

For the above example this will result in:

```haskell
module Main where

import Test.Hspec.Monadic

import qualified BazSpec
import qualified FooSpec
import qualified Foo.BarSpec

main :: IO ()
main = hspecX $ do
  describe "Foo" $ do
    FooSpec.spec
    describe "Bar"
      Foo.BarSpec.spec
  describe "Baz"
    BazSpec.spec
```

Other aspects `hspec-discover` are not yet configurable.  If you need anything
else, write me an email or [open an issue on GitHub]
(https://github.com/sol/hspec-discover/issues).

## Limitations

At the moment only monadic specs are supported.  It is possible to support
non-monadic specs, or even your custom specs through type classes.  Again, if
you need any of those, write me an email or [open an issue on GitHub]
(https://github.com/sol/hspec-discover/issues).
