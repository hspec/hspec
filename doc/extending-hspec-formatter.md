---
layout: default
title: "Extending Hspec: Writing a custom formatter"
---

This section describes how to customize the output of Hspec by writing a custom
formatter.  The focus is on how to write and package a formatter so that it can
be used by others.


## A simple formatter

The essence of a formatter in Hspec is a function `format :: Format`.  This
function takes an {{'Event'|id}} and produces some side effect:

```haskell
type Format = Event -> IO ()
```


As an example here is a format function that, on each completed spec item,
prints the item path and one of `✔`/`‐`/`✘`:

```haskell
format :: Format
format event = case event of
  ItemDone path item -> putStrLn (formatItem path item)
  _ -> return ()
  where
    formatItem :: Path -> Item -> String
    formatItem path item = joinPath path <> " [" <> formatResult item <> "]"

    formatResult :: Item -> String
    formatResult item = case itemResult item of
      Success {} -> "✔"
      Pending {} -> "‐"
      Failure {} -> "✘"

    joinPath :: Path -> String
    joinPath (groups, requirement) = intercalate " ❯ " $ groups ++ [requirement]
```

When we run a spec with this formatter we anticipate output of the form:

```
reverse ❯ reverses a list [✔]
reverse ❯ gives the original list, if applied twice [✔]
```

To use a formatter, we first need to register it; not the `format` function
itself, but something that offers more utility:

```haskell
formatter :: (String, FormatConfig -> IO Format)
formatter = ("my-formatter", \ _config -> return format)
```

- A formatter has a name so that it can be selected by `--format=NAME`.  Here, we use `"my-formatter"` as the name.
- Hspec passes a config value of type {{'FormatConfig'|id}} to the formatter so
  that the `format` function can make use of it.  Here, we don't
  look at the config value at all.
- Hspec allows the `format` function itself to be constructed in `IO`.  Here,
  we simply return the `format` function without doing any `IO`.

This `formatter` can then be used with {{'hspecWith'|id}}:

```haskell
main :: IO ()
main = hspecWith (useFormatter formatter defaultConfig) spec
```

{% note
A more idiomatic way to use formatters, that plays nice with `hspec-discover`,
is discussed below.
%}

{% example formatter/Simple.hs %}

## A monadic formatter

It is possible to use a custom monad stack for the `format` function.

As an example, let's assume we want to enumerate spec items, changing the
output from above to:

```
1. reverse ❯ reverses a list [✔]
2. reverse ❯ gives the original list, if applied twice [✔]
```

Using `StateT Int IO` instead of `IO` is one way to achieve this:

```haskell
format :: Event -> StateT Int IO ()
format event = case event of
  ItemDone path item -> do
    n <- state (id &&& succ)
    liftIO $ putStrLn (show n <> ". " <> formatItem path item)
  _ -> return ()
```

The {{'monadic'|id}} function can be used to transform a monadic `format` function
into one of type `IO Format`.  To do  this, {{'monadic'|id}} requires some function:

```haskell
run :: MonadIO m => m () -> IO ()
```

For `StateT Int IO` we can use {{'evalStateT'|id}}:

```haskell
formatter :: (String, FormatConfig -> IO Format)
formatter = ("my-formatter", \ _config -> monadic (`evalStateT` 1) format)
```

{% example formatter/Monadic.hs %}

## Using a custom formatter with \`hspec-discover\`

The previous examples used {{'hspecWith'|id}} to register a formatter.  This
works, but it requires the user to write a custom `main` function.  This is
inconvenient when you are using [`hspec-discover`](hspec-discover.html).

{{'modifyConfig'|id}} provides an alternative, it can be used to register a
formatter in {{'SpecM'|id}}:

```haskell
spec :: Spec
spec = do
  modifyConfig (useFormatter MyFormatter.formatter)
  ...
```

{{'modifyConfig'|id}} can be used anywhere within a spec.  However, it is idiomatic
to use it from a [spec hook](hspec-discover.html#spec-hooks):

```haskell
-- file test/SpecHook.hs
module SpecHook where

import           Test.Hspec
import           Test.Hspec.Api.Format.V1 (modifyConfig, useFormatter)
import qualified MyFormatter

hook :: Spec -> Spec
hook = (modifyConfig (useFormatter MyFormatter.formatter) >>)
```

## Packaging a formatter for distribution and reuse

When distributing a formatter you should, by convention, provide at least three
primitives:

```haskell
module MyFormatter (use, register, formatter, module Api) where

import Test.Hspec.Api.Format.V1 as Api

-- | Make `formatter` available for use with @--format@ and use it by default.
use :: SpecWith a -> SpecWith a
use = (modifyConfig (useFormatter formatter) >>)

-- | Make `formatter` available for use with @--format@.
register :: SpecWith a -> SpecWith a
register = (modifyConfig (registerFormatter formatter) >>)

formatter :: (String, FormatConfig -> IO Format)
formatter = ...
```

`use` and `register` make it convenient to register the formatter from a
[spec hook](hspec-discover.html#spec-hooks):

{% inline_source _includes/formatter/discover/ test/SpecHook.hs %}

The `formatter` should be exported so that it is possible to augment it if
needed.  In addition you may choose to export internals that are specific to
your formatter.

You also want to re-export `Test.Hspec.Api.Format.V1` for two reasons:

- It documents which version of the API is used by the formatter.
- It allows users to augment the formatter without having to import the correct
  version of the API themselves.

<div>
  <h5 class="foldable">Example code:</h5>
  <div>
{% inline_source _includes/formatter/discover/ src/MyFormatter.hs %}
{% inline_source _includes/formatter/discover/ test/SpecHook.hs %}
{% inline_source _includes/formatter/discover/ test/Data/ListSpec.hs %}
{% inline_example formatter/discover/test/Spec.hs %}
  </div>
</div>
