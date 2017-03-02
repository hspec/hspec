---
layout: default
---

## Using QuickCheck with Hspec

You can use arbitrary QuickCheck properties with Hspec, but they must be of
type {{'Property'|id}}.  QuickCheck's {{'property'|id}} function can be used to
turn anything that is a member of the {{'Testable'|id}} class into a
{{'Property'|id}}.  Here is an example:

```hspec
describe "read" $ do
  it "is inverse to show" $ property $
    \x -> (read . show) x == (x :: Int)
```

{% example QuickCheck.hs %}
