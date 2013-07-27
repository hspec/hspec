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

`before` is a function allowing you to add some custom `IO ()` action
before every test inside a spec. For example, if you have `flushDb`
which flushes your database, you could have this kind of
test-launcher:

```hspec
main :: IO ()
main = hspec $ before flushDb $ do
  describe "/api/users/" $ do
    it "creates a new user" $ do
      callApi "POST" "/api/users/" "name=jay"
      callApi "GET" "/api/users/count" `shouldReturn` 1
    it "ensures count of users is initially zero" $ do
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
