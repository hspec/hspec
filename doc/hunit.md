---
layout: default
title: Interoperability with HUnit
---


## Using HUnit assertions

Hspec's [expectation language](expectations.html) is built on top of
[HUnit](https://hackage.haskell.org/package/HUnit), hence it is possible to use
HUnit assertions instead.  E.g. `shouldBe` is just another name for HUnit's
{{'@?='|id}}.  You can use whatever you prefer and it will work
seamlessly.

{% example HUnit.hs %}


## Running a HUnit test suite with Hspec

{{'fromHUnitTest'|id}} from the
[`hspec-contrib`](https://hackage.haskell.org/package/hspec-contrib) package can
be used to convert a HUnit test suite to a `Spec`.  This can be used to run
existing HUnit tests with Hspec.  Ordinary spec items and HUnit tests can be
freely intermixed.

{% example HUnitTestSuite.hs %}
