---
layout: default
title: Passing options to Hspec
---

The previous section described how to
[create a test driver to run a spec](running-specs.html).  It is possible to customize how a spec is run by
passing *options* to that test driver.

## Supported options

You can get a list of supported options by passing `--help` to
the test driver:

<pre>
<kbd class="shell-input">runhaskell Spec.hs --help</kbd>
<samp>{{"_includes/Help.hs --help"|runhaskell}}</samp>
</pre>

## Specifying options on the command-line

How to specify command-line options depends on how you run your tests.  This
section describes how to pass command-line options to Hspec via Cabal and
Stack, and from withing a GHCi-session.

### Cabal

If you are using Cabal you can use `--test-option` to pass command-line options
to Hspec:

<pre><kbd class="shell-input">cabal test --test-show-details=direct --test-option=--format=progress</kbd></pre>
<pre><kbd class="shell-input">cabal test --test-show-details=direct --test-option=--match --test-option="some spec item"</kbd></pre>

{% note When running Hspec tests via Cabal, it is recommended to always specify `--test-show-details=direct`. %}

{% note With older versions of Cabal use `--show-details=direct` instead of `--test-show-details=direct`. %}

### Stack

If you are using `stack` you can use `--test-arguments` to pass command-line
options to Hspec:

<pre><kbd class="shell-input">stack test --test-arguments=--format=progress</kbd></pre>
<pre><kbd class="shell-input">stack test --test-arguments='--match "some spec item"'</kbd></pre>

### GHCi

If you run your tests from within a GHCi-session you can specify command-line
options right after `:main`:

```
*Main> :main --format=progress
```
```
*Main> :main --match "some spec item"
```

## Specifying options in config files

{% require 2.4.0 %}

Hspec reads options from two config files:

 - `~/.hspec` (a file named `.hspec` in the user's *home directory*)
 - `.hspec` (a file named `.hspec` in the *current directory*)

Example:
<pre><kbd class="shell-input">echo --format=progress >> .hspec</kbd></pre>
or
<pre><kbd class="shell-input">echo --format=progress >> ~/.hspec</kbd></pre>

## Specifying options through environment variables

{% require 2.8.4 %}

All of Hspec's options can be specified through environment variables:

<pre><kbd class="shell-input">HSPEC_FORMAT=progress cabal test --test-show-details=direct</kbd></pre>

For flags, like `--color`/`--no-color`, there is only one corresponding environment variable with valid values of `yes`/`no`:

<pre><kbd class="shell-input">HSPEC_COLOR=yes cabal test --test-show-details=direct</kbd></pre>

Option names are converted to environment variable names by:
 1. Converting all letters to uppercase
 2. Replacing all dashes with underscores
 3. Prepending `HSPEC_`

In addition, Hspec reads options from the environment variable `HSPEC_OPTIONS`. This is deprecated and will be removed at some point in the future.

## Precedence resolution for options that are specified in multiple places

If the same option is specified in multiple places, precedence is resolved as follows:

 - options from `~/.hspec` take the lowest precedence
 - options from `.hspec` take precedence over options from `~/.hspec`
 - options from environment variables take precedence over options from `.hspec`
 - command-line options take the highest precedence
