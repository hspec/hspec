---
layout: default
title: Using QuickCheck with Hspec
---

You can use arbitrary QuickCheck properties with Hspec, but they must be of
type {{'Property'|id}}.  QuickCheck's {{'property'|id}} function can be used to
turn anything that is a member of the {{'Testable'|id}} class into a
{{'Property'|id}}.  Here is an example:

```hspec
describe "read" $ do
  it "is inverse to show" $ property $
    \x -> (read . show) x `shouldBe` (x :: Int)
```

{% example QuickCheck.hs %}

It's also possible to modify some of the arguments passed to the Quickcheck
driver, namely: the maximum number of successes before succeeding, the maximum
number of discarded tests per successful test before giving up and size of
the test case:

```hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)

describe "read" $ do
  modifyMaxSize (const 1000) $ it "is inverse to show" $ property $
    \x -> (read . show) x `shouldBe` (x :: Int)
```
