---
layout: default
---

## Interoperability with HUnit

Hspec's [expectation language](expectations.html) is built on top of
[HUnit](http://hackage.haskell.org/package/HUnit), hence it is possible to use
HUnit assertions instead.  E.g. `shouldBe` is just another name for HUnit's
[`@?=`][assert-equal].  You can use whatever you prefer and it will work
seamlessly.

{% example HUnit.hs %}

[assert-equal]: http://hackage.haskell.org/packages/archive/HUnit/latest/doc/html/Test-HUnit-Base.html#v:-64--63--61-
