---
layout: default
---

# Parallel spec execution

You can use `parallel` to mark specs for parallel execution.  This is useful if
you want to speed up your tests by utilizing multiple CPUs.

Here is an example:

```hspec
-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Monad

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "expensiveOperation" $ do
    replicateM_ 4 $ do
      it "is expensive" $ do
        property expensiveOperation

expensiveOperation :: [[String]] -> Bool
expensiveOperation xs = (concat . concat) xs == (concat . concat) xs
```

Link the program with `-threaded` and pass `+RTS -N -RTS` when running it:

<pre>
<kbd class="shell-input">ghc -threaded Spec.hs</kbd>
</pre>


<pre>
<kbd class="shell-input">./Spec +RTS -N -RTS</kbd>
</pre>


All specs that are not explicitly marked with `parallel` are run in the
application's main thread.


By default, the number of threads available for parallel execution is equal to
the number of processors available to the process, as determined by
`+RTS -N`.  A different number (higher or lower) can be specified using the
`-j` flag.  Note that this number is in addition to the main thread.
