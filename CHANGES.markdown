## Changes in 1.1.0

### The reason for pending examples is now optional

With this change, both of the following code snippets work.

```haskell
it "some behaviour" $
  pending  -- no reason given
```

```haskell
it "some other behaviour" $
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
