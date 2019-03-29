## Changes in 2.7.1
  - Add compatibility with QuickCheck 2.13.1 (see #410)

## Changes in 2.7.0
  - Add `--focused-only` (see #390)
  - Add `--fail-on-focused` (see #389)

## Changes in 2.6.1
  - Add `Test.Hspec.Runner.runSpec` (see #216)

## Changes in 2.6.0
  - Allow to focus individual spec items (see #319)
  - Do not calculate diff on `--no-diff` (see #332)
  - Remove deprecated module Test.Hspec.Core

## Changes in 2.5.9
  - Extract source locations from RecConError (see #375)

## Changes in 2.5.8
  - Add `modifyArgs` and `modifyMaxShrinks` to `Test.Hspec.QuickCheck` (see #380)

## Changes in 2.5.7
  - GHC 8.6.1 related changes

## Changes in 2.5.6
  - Compatibility with `QuickCheck-2.12`

## Changes in 2.5.5
  - Use `module[line:column]` instead of `module:line:column` as default label
    for `describe`/`it` (fixes #366)

## Changes in 2.5.4
  - Show how to rerun individual spec items on test failures (see #205)

## Changes in 2.5.3
  - Treat character escapes like `\NUL` as single tokens on `--diff` (see #351)
  - Allow a `/` at the beginning and at the end of an absolute path that is
    passed to `--match` or `--skip`

## Changes in 2.5.2
  - Use module:line:column as default label for describe/it (see #250)
  - Warn if user is affected by https://ghc.haskell.org/trac/ghc/ticket/13285 (see #329)

## Changes in 2.5.1
  - Disable tests for Test.Hspec.Core.Timer (see #352)

## Changes in 2.5.0
  - Add `sequential` (see #311)
  - Add support for `--diff` when `shouldBe` is uesd with
    `QuickCheck`-properties
  - Add source locations when `shouldBe` is uesd with `QuickCheck` properties
  - Print `QuickCheck` labels on success (see #297)
  - Retain output of `verbose`, `label`, `collect`, `classify`, etc. for
    `QuickCheck` properties (see #257)
  - Extract source location from error / undefined (see #316)
  - Parse source locations from pattern match failures
  - Include source column when formatting source locations
  - Colorize whitespaces with background color instead of foreground color with
    `--diff`
  - Run `Test.Hspec.Core.Formatters.exampleProgress` in `FormatM` instead of
    `IO`
  - Make sure that progress output is always cleared (fixes #301)
  - Add location information to `pending` (not used by any formatter yet)
  - Include duration for each spec item in new formatter API (see #315) (not yet exposed)
  - Removed deprecated module `Test.Hspec.HUnit`, use
    `Test.Hspec.Contrib.HUnit` instead
  - Deprecate `--out`
  - Remove `BestEffort` source locations

## Changes in 2.4.8
  - compatibility with GHC 8.4.1-alpha3

## Changes in 2.4.7
  - compatibility with `QuickCheck-2.11.3` and up (note that `QuickCheck`
    versions `2.11` to `2.11.2` are not fully supported)

## Changes in 2.4.6
  - compatibility with the upcoming version `4.11.0.0` of `base`

## Changes in 2.4.5
  - `hspec-discover`: Sort specs using natural sort order

## Changes in 2.4.4
  - Require quickcheck-io >= 0.2.0

## Changes in 2.4.3
  - Read command-line options from environment variable `HSPEC_OPTIONS`

## Changes in 2.4.2
  - Use `--diff` by default
  - Add `--failure-report` (see #266)

## Changes in 2.4.1
  - Compatibility with HUnit < 1.3

## Changes in 2.4.0
  - Read command-line options from config files `~/.hspec` and `.hspec`
  - Add support for `--diff`
  - Add `xit`, `xspecify`, `xdescribe` and `xcontext` (see #252)
  - Add `--rerun-all-on-success`
  - Report exceptions in `beforeAll` operations only once
  - Add indentation when actual/expected contain newlines (see #263)
  - More graceful shutdown on ctrl-c (see #270)
  - Run around-hook for Bool and Result (see #252)
  - Include `CHANGES.markdown` in package tarball

Internal changes:

  - Rename `Fail` constructor of `Result`-type to `Failure`
  - Add `FailureReason` type
  - Add `Test.Hspec.Core.Spec.safeEvaluateExample`

## Changes in 2.3.2
  - Compatibility with HUnit 1.5

## Changes in 2.3.1
  - Fix build for HUnit < 1.4

## Changes in 2.3.0
  - Proper support for GHC 8 call stacks

## Changes in 2.2.4
  - Compatibility with QuickCheck-2.9

## Changes in 2.2.3
  - Make sure that `cabal haddock` works for `hspec-discover`
  - Forward compatibility for HUnit 1.4
  - Fix tests for GHC 8.0.1-rc1

## Changes in 2.2.2
  - Fix compilation for GHC 8.0.1-rc1

## Changes in 2.2.1
  - Make sure that Vim's default `errorformat` recognizes exact locations
  - GHCJS compatibility

## Changes in 2.2.0
  - Add source locations to test `Result`

## Changes in 2.1.10
  - GHC 7.0.* compatibility

## Changes in 2.1.9
  - Make use of GHC 7.10.2 source locations
  - Add `--jobs`

## Changes in 2.1.8
  - Depend on `hspec-expectations-0.7.0"

## Changes in 2.1.7
  - Add `beforeAll_`

## Changes in 2.1.6
  - If there were no previous failures, run all spec items on `--rerun`

## Changes in 2.1.5
  - Compatibility with QuickCheck-2.8

## Changes in 2.1.4
  - Make `hspec-discover` ignore modules with invalid module names, this fixes
    issues with `flycheck`'s temporary files

## Changes in 2.1.3
  - Format source locations like gcc does

## Changes in 2.1.2
  - Re-export `before_` from `Test.Hspec`

## Changes in 2.1.1
  - Add `before_`
  - Add command-line option `--skip`

## Changes in 2.1.0
  - Generalize `after_`, `afterAll_` and `around_`

## Changes in 2.0.2
  - Indent error messages for failed examples (fixes #186)
  - Export `defaultParams` from `Test.Hspec.Core.Example`
  - Bring back `Test.Hspec.HUnit` and deprecate it to provide a smother upgrade
    path

## Changes in 2.0.1
  - Add missing extra source files to for `hspec-discover`

## Changes in 2.0.0
  - Split package into `hspec`, `hspec-core` and `hspec-discover`
  - Allow hooks (`before`, `around`, etc.) to pass arguments to spec items
  - Do not print `-` in front of spec items with `specdoc` formatter
  - Move `Test.Hspec.HUnit` to `hspec-contrib`

## Changes in 1.12.4
  - Add `specGroup` and `specItem` to `Test.Hspec.Core`
  - Deprecate `Test.Hspec.Core.it` and `Test.Hspec.Core.describe`

## Changes in 1.12.3
  - Make `hspec-discover` work with `-XNoImplicitPrelude`

## Changes in 1.12.2
  - Include `IOErrorType` when printing uncaught `IOException`s (see #204)

## Changes in 1.12.1
  - Add `--module-name` option to `hspec-discover` (see #168)

## Changes in 1.12.0
  - Add optional source location to spec items
  - Move item requirement text from `SpecItem` constructor to `Item` data type
  - Remove `BuildSpecs` constructor from `SpecTree`, having `runIO` this is not
    really needed
  - Add `mapSpecTree` to `Test.Hspec.Core`
  - Add `afterAll` (see #188)
  - Do not return `Result` from `hspecWith` and accept command-line options
    (the old behavior is still available as `hspecWithResult`)
  - Rename `configHandle` to `configOutputFile`
  - Omit empty `describe` groups from report
  - Do not pass position to `Formatter.exampleGroupStarted` (we can not support
    this with the upcomming cleanup actions #188)
  - Do not print empty lines before/after spec groups
  - Deprecate `Test.Hspec.Formatters.newParagraph`

## Changes in 1.11.4
  - Make test suite independent from QuickCheck seed (see #187)

## Changes in 1.11.3
  - Depend on `hspec-expectations-0.6.1`

## Changes in 1.11.2
  - Add `beforeAll`

## Changes in 1.11.1
 - Add `specify` as an alias for `it`

## Changes in 1.11.0
 - Add `BuildSpecs` constructor to internal `SpecTree` data structure.  This
   allows you to do `IO` while constructing the spec tree.
 - Add `runIO`

## Changes in 1.10.0
 - Do not use exception type to distinguish between synchronous/asynchronous
   exceptions (thanks to Michael Snoyman)
 - Remove `Example` instance for `Test.HUnit.Test` (#101), use
   `Test.Hspec.HUnit.fromHUnitTest` instead.
 - Do not reexport `property` from `Test.Hspec.QuickCheck`
 - Move `ProgressCallback` out of `Params`
 - Add show instance for `Params`
 - Move requirement text from `Item` to `SpecItem` constructor
 - Remove deprecated modules and functions

## Changes in 1.9.5
 - Make sure that ctrl-c works properly with GHC 7.8.* and QuickCheck-2.6

## Changes in 1.9.4
 - Compatibility with transformers-0.4.0.0

## Changes in 1.9.3
 - Make internal Tree data structure more strict (#169)

## Changes in 1.9.2
 - Print type of exceptions that are raised from QuickCheck properties (#94)

## Changes in 1.9.1
 - Bring back compatibility with older versions of QuickCheck (#166)

## Changes in 1.9.0
 - Depend on QuickCheck 2.7

## Changes in 1.8.3
 - Do not use color if `TERM=dumb` (see #158)

## Changes in 1.8.2
 - `hspec-discover` now accepts `--no-main` which results in a top-level spec
   being generated instead of a test driver (thanks @DanielG)
 - Make sure that `after` is run on failing tests (see #159)

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
