---
layout: default
title: Setting expectations
---

Hspec provides several combinators that can be used to set expectations about
the outcome of code examples.

## Expecting equality

A common expectation is that two values are equal.  `shouldBe` can be used
here:

```haskell
x `shouldBe` 23
```

`shouldBe` can be used in combination with `IO` actions.  Here is an example:

```haskell
launchMissiles >>= (`shouldBe` Left "permission error")
```

`shouldReturn` is a shortcut for that.  The above code can be simplified to:

```haskell
launchMissiles `shouldReturn` Left "permission error"
```

## Require that a predicate holds

`shouldSatisfy` requires that some predicate holds for a given value.

```haskell
xs `shouldSatisfy` (not . null)
```

It is similar to HUnit's `assertBool`, but it gives a more detailed error
message if the expectation fails.

## Expecting exceptions

Hspec provides a mechanism to state that an action throws an exception.  Here
is a basic example:

```haskell
launchMissiles `shouldThrow` anyException
```

### Expecting exceptions of a specific type

The type of {{'shouldThrow'|id}} is:

```haskell
shouldThrow :: Exception e => IO a -> Selector e -> Expectation
```

It takes an `IO` action and a {{'Selector'|id}}.  The `Selector`
describes the precise nature of the expected exception.

There are several predefined selectors:

```haskell
anyException      :: Selector SomeException
anyErrorCall      :: Selector ErrorCall
anyIOException    :: Selector IOException
anyArithException :: Selector ArithException
```

The selector `anyException` can be used to expect an arbitrary exception.
Likewise `anyErrorCall`, `anyArithException` and `anyIOException` can be used
to expect arbitrary exceptions of type `ErrorCall`, `ArithException` and
`IOException` respectively.


### Using predicates as selectors

A `Selector` is just a predicate.  It can simultaneously constrain the type and
value of an exception.  This can be used in various ways, e.g. you can expect a
specific exception value:

```haskell
launchMissiles `shouldThrow` (== ExitFailure 1)
```

#### Expecting specific \`IOException\`s

The module `System.IO.Error` exports predicates to classify `IOException`s.
Those can be used in combination with `shouldThrow` to expect specific
`IOException`s.  Here is an example that uses
{{'isPermissionError'|id}} to require our hypothetic
`launchMissiles` action to fail with a permission error:

```haskell
launchMissiles `shouldThrow` isPermissionError
```

{% example IOExceptions.hs %}

#### Dealing with \`error\` and \`undefined\`

Both `error` and `undefined` throw exceptions of type
{{'ErrorCall'|id}}.
There is no `Eq` instance for `ErrorCall`, hence it is not possible to use `==`
to expect a specific exception value of type `ErrorCall`.  The following won't
work:

```haskell
evaluate (error "foo") `shouldThrow` (== ErrorCall "foo")  -- This won't work!
```

Pattern matching can be used instead, but Hspec provides a combinator,
{{'errorCall'|id}}, to make this more convenient.  Here is how it's
used:

```haskell
evaluate (error "foo") `shouldThrow` errorCall "foo"
```

{% example Error.hs %}

### Expecting exceptions from pure code

{{'evaluate'|id}} can be used to expect exceptions from pure code:

```hspec
evaluate (1 `div` 0) `shouldThrow` anyArithException
```

However, `evaluate` only forces its argument to _weak head normal form_.  To
better understand what that means, let's look at an other example:

```hspec
evaluate ('a' : undefined) `shouldThrow` anyErrorCall
```

Here `evaluate` does not force the exception.  It only evaluates its argument
until it encounters the first constructor.  Here the constructor is `:`, as soon
as `evaluate` sees `:` it's done.  It does not look at the arguments of that
constructor.

{{'force'|id}} can be used to force the exception:

```hspec
(evaluate . force) ('a' : undefined) `shouldThrow` anyErrorCall
```

Note that `return $!!` (or any other mechanism that relies solely on `seq`)
does not work reliably!  (see the discussion at
<del>[#5129](https://gitlab.haskell.org/ghc/ghc/-/issues/5129)</del>)

{% example ExceptionsFromPureCode.hs %}

#### Beware of GHC's _semantics for imprecise exceptions_

GHC's semantics for imprecise exceptions can be tricky.  But simple examples
are evident.  If we look at the _exceptional value_ `error "foo" + error "bar"` it may
be tempting to assume that the following expectation holds:

```hspec
evaluate (error "foo" + error "bar" :: Int) `shouldThrow` errorCall "foo"
```

There is a pretty good chance that this will indeed hold, but it may equally
well fail.  The reason is that `+` does not give any guarantees about the
evaluation order of its arguments.

Semantically an exceptional value contains a set of exceptions.  When we "look"
at the value, one _representative_ from the set is chosen in a
non-deterministic way.

For our simple example the set of exceptions is `{ErrorCall "foo", ErrorCall
"bar"}`, so we have to be prepared to encounter any of those.  We can
accommodate for this by combining `errorCall "foo"` and `errorCall "bar"` with
`||`:

```hspec
evaluate (error "foo" + error "bar" :: Int)
  `shouldThrow` (||) <$> errorCall "foo" <*> errorCall "bar"
```

The details of imprecise exceptions are described in the paper
[_A semantics for imprecise exceptions_]
(https://www.microsoft.com/en-us/research/publication/a-semantics-for-imprecise-exceptions/).
But beware that GHC does not fully adhere to those semantics (see
[#1171](https://ghc.haskell.org/trac/ghc/ticket/1171),
[#2273](https://ghc.haskell.org/trac/ghc/ticket/2273),
<del>[#5561](https://ghc.haskell.org/trac/ghc/ticket/5561)</del>,
<del>[#5129](https://ghc.haskell.org/trac/ghc/ticket/5129)</del>).

{% example ImpreciseExceptions.hs %}
