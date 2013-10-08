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

See [Running tests with Hspec](running-specs.html)
