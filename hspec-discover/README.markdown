### Getting started

> For the purpose of this example all source files are put into the `src/`
> directory, specs are put into the `test/` directory, and the `Main` module of
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
Now, to run all the specs.

```haskell
-- file test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

This invokes `hspec-discover` when the module is compiled or loaded into
`ghci`, and generates a suitable `Main`.

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

### Customizing things

If rather want to map hierarchical modules to a nested spec, pass `--nested` to
`hspec-discover`.

```haskell
-- file Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --nested #-}
```

For the example above this will result in:

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

### Limitations

It currently only works for monadic specs.  It is possible to support
non-monadic specs, or even your custom specs through type classes.  If you need
any of those, write me an email or [open an issue on GitHub]
(https://github.com/sol/hspec-discover/issues).
