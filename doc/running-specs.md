---
layout: default
title: Running tests with Hspec
---

To run Hspec tests we have to define an executable, the *test driver*.
By convention, in the context of a *Haskell package*, we define the test driver
in `test/Spec.hs`.
As we
have already seen in previous sections of this manual, the {{'hspec'|id}}
function can be used to do this.  In general a `spec` of type `Spec` can be
turned into an executable with:

```hspec
-- file test/Spec.hs
module Main where

import Tests.Hspec

main :: IO ()
main = hspec spec
```

{% note For a real-world project [the test driver can be generated with hspec-discover](hspec-discover.html). %}
