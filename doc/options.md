---
layout: default
title: Passing options to Hspec
---

In the last section we have seen how to [create a test driver for your tests](running-specs.html).
It is possible to customize how a spec is run by passing *options* to that test
driver.

{{ page.content | toc }}


## Supported options

You can get a list of supported options by passing `--help` to
the test driver:

<pre>
<kbd class="shell-input">runhaskell Spec.hs --help</kbd>
<samp>{{"_includes/Help.hs --help"|runhaskell}}</samp>
</pre>

## Specifying options on the command-line

How to specify command-line options depends on how your run your tests.  This
section describes how to pass command-line options to Hspec via Cabal and
Stack, and from withing a GHCi-session.

### Cabal

If you are using Cabal you can use `--test-option` to pass command-line options
to Hspec:

<pre><kbd class="shell-input">cabal test --show-details=direct --test-option=--format=progress</kbd></pre>

***Note:*** When running Hspec tests via Cabal, it is recommended to always specify `--show-details=direct`.


### Stack

If you are using `stack` you can use `--test-arguments` to pass command-line
options to Hspec:

<pre><kbd class="shell-input">stack test --test-arguments=--format=progress</kbd></pre>

### GHCi

If you run your tests from within a GHCi-session you can specify command-line
options right after `:main`:

```
*Main> :main --format=progress
```

## Specifying options in config files


Hspec reads options from two config files:

 - `~/.hspec` (a file named `.hspec` in the user's *home directory*)
 - `.hspec` (a file named `.hspec` in the *current directory*)

Example:
<pre><kbd class="shell-input">echo --format=progress >> .hspec</kbd></pre>
or
<pre><kbd class="shell-input">echo --format=progress >> ~/.hspec</kbd></pre>

## Specifying options through an environment variable

Hspec reads options from the environment variabe `HSPEC_OPTIONS`:

<pre><kbd class="shell-input">HSPEC_OPTIONS="--format=progress" cabal test --show-details=direct</kbd></pre>

## Precedence resolution for options that are specified in multiple places

If the same option is specified in multiple places, precedence is resolved as follows:

 - options from `~/.hspec` take the lowest precedence
 - options from `.hspec` take precedence over options from `~/.hspec`
 - options from `HSPEC_OPTIONS` take precedence over options from `.hspec`
 - command-line options take the highest precedence
