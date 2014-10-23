---
layout: default
---

# User's Manual

Hspec is a testing framework for [Haskell](http://www.haskell.org/).  It is
inspired by the Ruby library [RSpec](http://rspec.info/).  Some of Hspec's
distinctive features are:

 * a friendly DSL for defining tests
 * integration with QuickCheck, SmallCheck, and HUnit
 * parallel test execution
 * automatic discovery of test files

An example is worth a thousand words, so here we go:

<pre><kbd class="shell-input">cabal update && cabal install hspec</kbd></pre>

{% inline_example Example.hs %}

## Table of contents

* [Five-step introduction](getting-started.html)
* [Writing tests with Hspec](writing-specs.html)
* [Running tests with Hspec](running-specs.html)
* [Setting expectations](expectations.html)
* [Using QuickCheck with Hspec](quickcheck.html)
* [Interoperability with HUnit](hunit.html)
* [Automatic spec discovery](hspec-discover.html)
* [Parallel spec execution](parallel-spec-execution.html)

## Extensions

* [hspec-expectations-lens](http://hackage.haskell.org/package/hspec-expectations-lens) — Hspec expectations for the _lens_ stuff
* [hspec-webdriver](http://hackage.haskell.org/package/hspec-webdriver) — write end2end web application tests using _webdriver_ and hspec
* [hspec-snap](http://hackage.haskell.org/package/hspec-snap) — a library for testing with Hspec and the _Snap Web Framework_
* [hspec-checkers](http://hackage.haskell.org/package/hspec-checkers) — allows to use _checkers_ properties from hspec
* [hspec-smallcheck](http://hackage.haskell.org/package/hspec-smallcheck) — support for _SmallCheck_
* [hspec-laws](http://hackage.haskell.org/package/hspec-laws) — document and test laws for standard type classes
* [hspec-attoparsec](http://hackage.haskell.org/package/hspec-attoparsec) — utility functions for testing your _attoparsec_ parsers with hspec
* [hspec-jenkins](http://hackage.haskell.org/package/hspec-jenkins) — Jenkins-friendly XML formatter for Hspec
* [hspec-expectations-lifted](http://hackage.haskell.org/package/hspec-expectations-lifted) — a version of _hspec-expectations_ generalized to MonadIO

## Blogosphere

* [Writing controller specs for a Warp server](http://begriffs.com/posts/2014-10-19-warp-server-controller-test.html)
* [Testing attoparsec parsers with hspec](http://alpmestan.com/posts/2014-06-18-testing-attoparsec-parsers-with-hspec.html)

## Other resources

* [Hspec on GitHub](https://github.com/hspec/hspec)
* [Hspec on Hackage](http://hackage.haskell.org/package/hspec)
* [API documentation](http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec.html)
* [Vim syntax file for Hspec](https://github.com/hspec/hspec.vim#readme)
* [Guard plugin for Hspec](http://rubygems.org/gems/guard-haskell)

## Support

Want to chat? `/join #hspec` on freenode!
