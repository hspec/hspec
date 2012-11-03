---
layout: default
---

## Writing Tests with Hspec

### Using \`describe\` and \`it\`

Hspec provides an EDSL to describe tests.

`describe` and `it` are used to organize test with Hspec.

```hspec
main :: IO ()
main = hspec $ do
  describe "Prelude.read" $ do
    it "can parse integers" $ do
      read "10" `shouldBe` (10 :: Int)

    it "can parse floating-point numbers" $ do
      read "2.5" `shouldBe` (2.5 :: Float)
```

A spec can consist of multiple `describe`s, and `describe`s can be nested.

```hspec
main :: IO ()
main = hspec $ do
  describe "Prelude" $ do
    describe "read" $ do
      it "can parse integers" $ do
        read "10" `shouldBe` (10 :: Int)

    describe "head" $ do
      it "returns the first element of a list" $ do
        head [23 ..] `shouldBe` 23
```

### Using \`context\`

`context` is just an alias for `describe`.  Use `context` with *when* or
*with*, e.g.:


```hspec
main :: IO ()
main = hspec $ do
  describe "parse" $ do
    context "when provide with invalid input" $ do
      it "returns a parse error" $ do
        parse "some invalid input" `shouldBe` Left "parse error"
```

### Running specs

The most common way to run a spec is with `hspec`, e.g.:

```
main = hspec spec
```

You can customize how a spec is run by either providing command-line flags or
by using `hspecWith` instead of `hspec`.

*Note: `hspecWith` is only available in the upcoming 1.4 release of Hspec.*
