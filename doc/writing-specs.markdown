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

### Using hooks: \`before\`, \`after\`, \`around\`

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

Similarly, `after` runs a custom `IO` action after every spec item:

```hspec
main :: IO ()
main = hspec $ after truncateDatabase $ do
  describe "createUser" $ do
      it "creates a new user" $ do
        let eva = User (UserId 3) (Name "Eva") (Age 28)
        createUser eva
        getUser (UserId 3) `shouldReturn` eva
  describe "countUsers" $ do
    it "counts all registered users" $ do
      countUsers `shouldReturn` 0
```

`around` is passed an `IO` action for each spec item so that it can perform
whatever setup and teardown is necessary.

```hspec
withStubbedApi :: IO () -> IO ()
withStubbedApi action = do
  server <- serveStubbedApi "localhost" 80
  action
  stopServer server

main :: IO ()
main = hspec $ around withStubbedApi $ do
  describe "api client" $ do
    it "should authenticate" $ do
      c <- newClient (Just ("user", "pass"))
      get "/api/auth" `shouldReturn` http200
    it "should allow anonymous access" $ do
      c <- newClient Nothing
      get "/api/dogs" `shouldReturn` http200
```

Hooks do not support passing values to spec items (for example, if you wanted
to open a database connection before each item and pass the connection in).
However this shouldn't be a problem; you can just do something like this:

```hspec
openConnection :: IO Connection
openConnection = ...

closeConnection :: Connection -> IO ()
closeConnection = ...

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

spec :: Spec
spec = do
  describe "createRecipe" $ do
      it "creates a new recipe" $ withDatabaseConnection $ \c -> do
        let ingredients = [Eggs, Butter, Flour, Sugar]
        createRecipe c (Recipe "Cake" ingredients)
        getRecipe c "Cake" `shouldReturn` ingredients
```

### Using \`pending\` and \`pendingWith\`

`pending`/`pendingWith` can be used as a kind of TODO list while you are
writing your specs.  If you think of some behaviour that your code should
satisfy, but you are working on something else, you can add a spec item and
just set it to pending.

```hspec
main :: IO ()
main = hspec $ do
  describe "/login" $ do
    it "should use correct status codes" $ do
      pending
    it "should require basic authentication" $ do
      pendingWith "need to fix base64 first"
```

It's advisable to resolve all pending specs before committing your changes.

The report produced at the end of testing counts passed, failed, and pending
specs separately, and having unresolved pending specs does not cause your test
suite to fail; that is, it can still exit with a status code of 0 if there are
unresolved pending specs.
