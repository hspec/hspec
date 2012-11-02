---
layout: default
---

## Using QuickCheck with Hspec

You can use arbitrary QuickCheck properties with Hspec, but they must be
wrapped with QuickCheck's [`property`][v:property] function.  Here is an
example:

```hspec
describe "read" $ do
  it "is inverse to show" $ property $
    \x -> (read . show) x == (x :: Int)
```


{% example QuickCheck.hs %}

[t:Property]: http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck.html#t:Property
[t:Testable]: http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck-Property.html#t:Testable
[v:property]: http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck.html#v:property
