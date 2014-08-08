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

## Other resources

* [Hspec on GitHub](https://github.com/hspec/hspec)
* [Hspec on Hackage](http://hackage.haskell.org/package/hspec)
* [API documentation](http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec.html)
* [SmallCheck integration](http://hackage.haskell.org/package/hspec-smallcheck)
* [Vim syntax file for Hspec](https://github.com/hspec/hspec.vim#readme)
* [Guard plugin for Hspec](http://rubygems.org/gems/guard-haskell)
* [A report of Hspec's behavior](report.html)

## Support

Want to chat? `/join #hspec` on freenode!
