## Running specific examples

You can run examples that match a given pattern by passing `--match PATTERN` to
your test driver.

### Example

Create a spec

```haskell
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "foo" $ do
    describe "bar" $ do
      it "example 1" False
      it "example 2" True

  describe "baz" $ do
    it "example 3" True
```

and load it into GHCi.

Now if you run it with

```
ghci> :main --match foo/bar
```

`example 1` and `example 2` will be run.


If you run it with

```
ghci> :main --match "foo/bar/example 1"
```
instead, only `example 1` is run.

In addition, you can also use the message that is given for a failing example
after the each test run.  E.g. the following would work instead:

```
ghci> :main --match "foo.bar example 1"
```
