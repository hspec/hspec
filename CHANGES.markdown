## Changes in 1.9.0
 - Depend on QuickCheck 2.7

## Changes in 1.8.3
 - Do not use color if `TERM=dumb` (see #158)

## Changes in 1.8.2
 - `hspec-discover` now accepts `--no-main` which results in a top-level spec
   being generated instead of a test driver (thanks @DanielG)
 - Make sure that after is run on failing tests (see #159)

## Changes in 1.8.1
 - Add `shouldMatchList`

## Changes in 1.8.0
 - Run `before`/`after`/`around` for each single check of a QuickCheck property
 - Add `Test.Hspec.Core.mapSpecItem`
 - Add `modifyMaxSuccess`, `modifyMaxDiscardRatio` and `modifyMaxSize` to
   `Test.Hspec.QuickCheck`
 - Don't fail if callback is not called in `around`
 - `hspec-discover`: Remove `--nested` option
 - `hspec-discover`: Ignore `Spec.hs`/`Spec.lhs`

## Changes in 1.7.2
 - Add `after` and `around`

## Changes in 1.7.1
 - Add `shouldContain` (thanks to Alfredo Di Napoli)
 - When printing progress, skip total if it is 0
 - Do not colorize the description/requirement in failure list

## Changes in 1.7.0
 - Add `--depth` for use with `hspec-smallcheck`

## Change in 1.6.2
 - Add `before`
 - Add `--qc-max-discard` and `--qc-max-size`

## Changes in 1.6.1
 - Allow to specify the output file with `--out`

## Changes in 1.6.0
 - Add support for parallelization
 - Change license to MIT
 - Add MonadIO instance for FormatM
 - Add support for custom formatters to hspec-discover
 - Add hspecResult
 - Rename `--re-run` to `--rerun` + add documentation (#95)
 - Remove `configVerbose`
 - Use same `--qc-max-success` on `--rerun` (#125)
 - Add command-line option `--no-color`, `--color` does not accept arguments
   anymore (#130)

## Changes in 1.5.4
 - Make sure that QuickCheck is never chatty
 - Make sure progress for QuickCheck examples is shown

## Changes in 1.5.3
 - Print "Randomized with seed ..." only once

## Changes in 1.5.2
 - Add `--seed`, it can be used to specify the seed for QuickCheck properties
 - Reuse previous seed on `--re-run`

## Changes in 1.5.1
 - Depend on quickcheck-io

## Changes in 1.5.0
 - Allow to use expectations as QuickCheck properties (#80)
 - Do not suppress output to `stdout`
 - Change type of `pending` to `Expectation`, add `pendingWith` (#121)
 - Add the `example` function, it fixes the type of an Expectation (#86)
 - Rename `--fast-fail` to `--fail-fast` (for consistency with RSpec)
 - Do not clutter Cabal test suite logs with QuickCheck output (#112)
 - Skip redundant output from QuickCheck failure messages (#102)

## Changes in 1.4.5
 - hspec-discover now discovers .lhs files, too

## Changes in 1.4.4
 - Visually distinguish error message from requirements in the summary (#100)
 - Export `formatException` from `Test.Hspec.Formatters`
 - Add `--fast-fail` (#82)
 - Print a summary on UserInterrupt/ctrl-c (#107)

## Changes in 1.4.3
 - Add `--dry-run` (#111)

## Changes in 1.4.2
 - Properly handle ctrl-c while running QuickCheck properties (#93)
 - Default to `--color=always` when `--color` is used without argument (#106)
 - treat `--qc-max-success` as an alias for `--maximum-generated-tests`

## Changes in 1.4.1
 - Used CPU time is now only included in the test summary if run with
   `--print-cpu-time`.

## Changes in 1.4.0
 - We now have a manual at http://hspec.github.io/.  The sources are in
   `doc/`.  It's still work in progress.  Contributions are very welcome!
 - The Haddock documentation now indicates the stability of each exposed
   module.  `Test.Hspec` is now considered stable.
 - `hspec` now supports command-line options
   - `--color` can be used to enable/disable colored output
   - `--format` can be used to pick a specific formatter
   - `--html` can be used to produce an HTML report
   - `--maximum-generated-tests` can be used to specify QuickCheck's
     `maxSuccess`
   - `--match` only runs spec items that match a given string
   - `--re-run` only runs spec items that previously failed.  This is
     undocumented,
     experimental and only works within GHCi (use `:reload` / `:main`)!
 - Runner functions exported from `Test.Hspec.Runner` new expect a monadic
   spec.
 - `fromHUnitTest` has been added.  It can be used to run existing HUnit test
   suites with Hspec.
 - The `Example` instance for HUnit `Test`s has been deprecated.
   `fromHUnitTest` can be used instead.  The primary motivation is, that
   `fromHUnitTest` gives more detailed reporting for nested HUnit test suites.
 - `Test.Hspec.Monadic` has been deprecated, all functionality is available
   through one of `Test.Hspec`, `Test.Hspec.Core` or `Test.Hspec.Runner`.
 - More of Hspec's internals are now exposed from `Test.Hspec.Core`
 - All runner functions for the core spec type (aka as non-monadic spec) have
   been deprecated, use e.g. `Test.Hspec.hspec . fromSpecList` instead.  The
   motivation is to provide an API that does not expose colliding names.
 - Some other stuff from `Test.Hspec.Core` that collides with other parts of
   the API has been deprecated.  Compatible alternatives are given in each
   deprecation message.
 - The default formatter now produces less whitespace (#73) + other minor
   improvements
 - The formatter API has been revamped.
 - The exception type is now printed if an example fails due to an exception
   (#50)
 - The number of pending examples is now printed after each test run (#85)
 - `--verbose` has been added (#87)

## Changes in 1.3.0

 - `Test.Hspec` now re-exports the monadic API.  If you still use the
   non-monadic API, you can use `Test.Hspec.Core` as a drop-in replacement.

 - `hspec-expectations`, a set of combinators that allow to express
   expectations about the outcome of code examples, is now included with Hspec.
   Have a look at the [README of `hspec-expectations`]
   (https://github.com/sol/hspec-expectations#readme)
   for a short introduction.

 - `hspec-discover`, a mechanism for automatic spec discovery, is now an
   official part of Hspec.
   Have a look at [`hspec-discover`'s README]
   (https://github.com/hspec/hspec/tree/master/hspec-discover#readme)
   for a short introduction.

## Changes in 1.2.0

 - `hspec` has been removed, and `hspecX` has been renamed to `hspec` (see
   [#71](https://github.com/hspec/hspec/issues/71))

 - `hHspec` now returns a summary of the test run.

 - The time reporting after a test run is not colored anymore.

## Changes in 1.1.3

 - `Test.Hspec` warns now about future changes: It will re-export
   `Test.Hspec.Monadic` in the future.  If you still use the non-monadic API,
   you can either use `Test.Hspec.Core` as a drop-in replacement, or migrate your
   code to the monadic API.

   The monadic API is more stable and easier to use.  Now is a good time to
   switch!

 - `Test.Hspec.Core` is now a proper superset of `Test.Hspec`

## Changes in 1.1.2

 * All descriptions of nested examples are now included in summary for failing
   examples

 * `context` is now an alias for `describe`

## Changes in 1.1.1

 * Specs from Test.Hspec.Monadic has been renamed to Spec.  For backward
   compatibility Specs is still kept as an alias.

## Changes in 1.1.0

### The reason for pending examples is now optional

With this change, both of the following code snippets work.

```haskell
it "some behavior" $
  pending  -- no reason given
```

```haskell
it "some other behavior" $
  pending "some reason"
```

### Hspec does not rely on ExistentialQuantification anymore

### The type used to represent specs is now abstract

This should give more useful error messages when adapting old specs that use
the non-monadic API for `hspec-1.0`/`hspec-1.1`.

### Several internal types and functions have been deprecated

Those are internal functions, and they will be removed/hidden with the next
release.  If you use any of those, update your code.  If you really need them,
[open a ticket](https://github.com/hspec/hspec/issues) and describe your use
case.

## Changes in 1.0.0

### Hspec now re-uses QuickCheck's property function

`Test.Hspec.QuickCheck.property` is now simply a re-exports of
`Test.QuickCheck.property`.  This has the advantage that you do not get a name
collision if you import both, `Test.Hspec.QuickCheck` and `Test.QuickCheck`.

### Better support for nested specs

*NOTE: This is a breaking change to the non-monadic API.  The monadic API is
not affected.*

In some situations parent descriptions for nested specs were not included in
the generated report.  Solving this required a change to the data structure
that is used to represent specs (it was not a proper tree, now it is).

#### Updating specs that use the non-monadic API

The runner functions (`hspec`, `hspecB` and `hspecX`) now take a list of
descriptions.

The following works with `hspec-0.9`, but not with `hspec-1.0`.

```haskell
main = hspecX $
  describe "reverse" [
    it "reverses a list" $
      reverse [1, 2, 3] == [3, 2, 1],

    it "gives the original list, if applied twice" $ property $
      \xs -> reverse (reverse xs) == (xs :: [Int])
  ]

```

For `hspec-1.0`, you need to wrap it into a list.

```haskell
main = hspecX [
    describe "reverse" [
      it "reverses a list" $
        reverse [1, 2, 3] == [3, 2, 1],

      it "gives the original list, if applied twice" $ property $
        \xs -> reverse (reverse xs) == (xs :: [Int])
    ]
  ]
```

Specs consisting of several *desribes*, combined with `descriptions`, continue
to work unchanged.  But `descriptions` is now a noop, and it will be removed in
a future release.  So it is a good idea to drop it.

The following works with both `hspec-0.9` and `hspec-1.0`.

```haskell
main = hspecX $ descriptions [  -- descriptions is redundant
    describe "Foo" [
      it "has some behavior" True
    ]
  , describe "Bar" [
      it "has some behavior" True
    ]
  ]
```

But the following is recommended instead.

```haskell
main = hspecX [
    describe "Foo" [
      it "has some behavior" True
    ]
  , describe "Bar" [
      it "has some behavior" True
    ]
  ]
```

### A new monadic API for custom Formatters

For all the details, have a look at the [docs]
(http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec-Formatters.html).


### The total time required to run a spec is now included in the summary

In addition to the used CPU time, the total time required to run a spec is now
include in the summary.  This is useful for specs that do non-CPU-intensive
stuff, or fork subprocesses.
