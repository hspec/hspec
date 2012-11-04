---
layout: default
---

## Using QuickCheck with Hspec

You can use arbitrary QuickCheck properties with Hspec, but they must be of
type [`Property`][t:Property].  QuickCheck's [`property`][v:property] function
can be used to turn anything that is a member of the [`Testable`][t:Testable]
class into a `Property`.  Here is an example:

```hspec
describe "read" $ do
  it "is inverse to show" $ property $
    \x -> (read . show) x == (x :: Int)
```


{% example QuickCheck.hs %}

[t:Property]: http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck.html#t:Property
[t:Testable]: http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck-Property.html#t:Testable
[v:property]: http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck.html#v:property
