---
layout: default
---

## Expecting exceptions

Hspec provides a mechanism to state that an action throws an exception.  Here
is a basic example:

```haskell
launchMissiles `shouldThrow` anyException
```

### Expecting exceptions of a specific type

The type of [`shouldThrow`][v:shouldThrow] is:

```haskell
shouldThrow :: Exception e => IO a -> Selector e -> Expectation
```

It takes an `IO` action and a [`Selector`][t:Selector].  The `Selector`
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

#### Expecting specific `IOException`s

The module `System.IO.Error` exports predicates to classify `IOException`s.
Those can be used in combination with `shouldThrow` to expect specific
`IOException`s.  Here is an example that uses
[`isPermissionError`][v:isPermissionError] to require our hypothetic
`launchMissiles` action to fail with a permission error:

```haskell
launchMissiles `shouldThrow` isPermissionError
```

{% extended_example IOExceptions.hs %}

#### Dealing with \`error\` and \`undefined\`

Both `error` and `undefined` throw exceptions of type
[`ErrorCall`][t:ErrorCall].
There is no `Eq` instance for `ErrorCall`, hence it is not possible to use `==`
to expect a specific exception value of type `ErrorCall`.  The following won't
work:

```haskell
error "foo" `shouldThrow` (== ErrorCall "foo")  -- This won't work!
```

Pattern matching can be used instead, but Hspec provides a combinator,
[`errorCall`][v:errorCall], to make this more convenient.  Here is how it's
used:

```haskell
error "foo" `shouldThrow` errorCall "foo"
```

{% extended_example Error.hs %}

### Expecting exceptions from pure code

[`evaluate`][v:evaluate] can be used to expect exceptions from pure code:

```hspec
evaluate (1 `div` 0) `shouldThrow` anyArithException
```

However, `evaluate` only forces its argument to _weak head normal form_.  To
better understand what that means, let's look at an other example:

```hspec
evaluate ('a' : undefined) `shouldThrow` errorCall "Prelude.undefined"
```

Here `evaluate` does not force the exception.  It only evaluates `'a' :
undefined` until it encounters the first constructor &mdash; the `:` &mdash; and is done.
It does not look at the arguments of that contructor.

[`$!!`][v:deep-apply] can be used to force the exception:

```hspec
(return $!! 'a' : undefined) `shouldThrow` errorCall "Prelude.undefined"
```

{% extended_example ExceptionsFromPureCode.hs %}

#### Beware of GHC's _semantics for imprecise exceptions_

GHC's semantics for imprecise exceptions can be tricky.  But simple examples
are evident.  If we look at the _exceptional value_ `error "foo" + error "bar"` it may
be tempting to assume that the following expectation holds:

```hspec
evaluate (error "foo" + error "bar" :: Int) `shouldThrow` errorCall "foo"
```

There is a pretty good chance that this will indeed hold, but it may equally
well fail.  The reason is that `+` does not give any guarantees about the
evaluation order of it's arguments.

Semantically an exceptional value contains a set of exceptions.  When we "look"
at the value, one _representative_ from the set is chosen in a
non-deterministic way.
For our simple example we can accommodate for that by combining `errorCall
"foo"` and `errorCall "bar"` with `||`:

```hspec
evaluate (error "foo" + error "bar" :: Int)
  `shouldThrow` (||) <$> errorCall "foo" <*> errorCall "bar"
```

The details of imprecise exceptions are described in the paper
[_A semantics for imprecise exceptions_]
(http://research.microsoft.com/en-us/um/people/simonpj/papers/imprecise-exn.htm).
But beware that GHC does not fully adhere to those semantics (see
[#1171](http://hackage.haskell.org/trac/ghc/ticket/1171),
[#2273](http://hackage.haskell.org/trac/ghc/ticket/2273),
<del>[#5561](http://hackage.haskell.org/trac/ghc/ticket/5561)</del>,
<del>[#5129](http://hackage.haskell.org/trac/ghc/ticket/5129)</del>).

{% extended_example ImpreciseExceptions.hs %}



[t:Selector]:    http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#t:Selector
[v:shouldThrow]: http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#v:shouldThrow
[v:errorCall]:   http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#v:errorCall

[v:evaluate]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception.html#v:evaluate
[t:ErrorCall]:http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception.html#t:ErrorCall

[v:isPermissionError]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO-Error.html#v:isPermissionError

[v:deep-apply]:  http://hackage.haskell.org/packages/archive/deepseq/latest/doc/html/Control-DeepSeq.html#v:-36--33--33-
