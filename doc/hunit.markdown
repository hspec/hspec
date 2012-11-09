---
layout: default
---

## Interoperability with HUnit

### Using HUnit assertions

Hspec's [expectation language](expectations.html) is built on top of
[HUnit](http://hackage.haskell.org/package/HUnit), hence it is possible to use
HUnit assertions instead.  E.g. `shouldBe` is just another name for HUnit's
[`@?=`][assert-equal].  You can use whatever you prefer and it will work
seamlessly.

{% example HUnit.hs %}

[assert-equal]: http://hackage.haskell.org/packages/archive/HUnit/latest/doc/html/Test-HUnit-Base.html#v:-64--63--61-

### Running a HUnit test suite with Hspec

Hspec's [`fromHUnitTest`][v:fromHUnitTest] can be used to convert a HUnit test
suite to a `Spec`.  This can be used to run existing HUnit tests with Hspec.
Ordinary spec items and HUnit tests can be freely intermixed.

{% example HUnitTestSuite.hs %}

[v:fromHUnitTest]: http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec-HUnit.html#v:fromHUnitTest
