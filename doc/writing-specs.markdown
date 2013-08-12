---
layout: default
---

## Writing Tests with Hspec

### Using \`describe\` and \`it\`

Hspec provides an EDSL to describe tests.

`describe` and `it` are used to organize tests with Hspec.

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
    context "when provided with invalid input" $ do
      it "returns a parse error" $ do
        parse "some invalid input" `shouldBe` Left "parse error"
```

### Using \`before\`

`before` runs a custom `IO` action before every spec item. For example, if you
have an action `flushDb` which flushes your database, you can run it before
every spec item with:

```hspec
main :: IO ()
main = hspec $ before flushDb $ do
  describe "/api/users/count" $ do
    it "returns the number of users" $ do
      post "/api/users/create" "name=Jay"
      get "/api/users/count" `shouldReturn` 1
    context "when there are no users" $ do
      it "returns 0" $ do
        callApi "GET" "/api/users/count" `shouldReturn` 0
```

### Running specs

The most common way to run a spec is with {{'hspec'|id}}, e.g.:

```
main = hspec spec
```

It is possible to customize how a spec is run by providing command-line flags.
You can get a list of supported flags by passing `--help` to your test driver:

<pre>
<kbd class="shell-input">runhaskell Spec.hs --help</kbd>
<samp>{{"_includes/Help.hs --help"|runhaskell}}</samp>
</pre>

Using {{'hspecWith'|id}} instead of `hspec` gives even more control over how a
spec is run.
