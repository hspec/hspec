module Test.Hspec.Core.HooksSpec (main, spec) where

import           Prelude ()
import           Helper

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Runner as H

import qualified Test.Hspec.Core.Hooks as H

main :: IO ()
main = hspec spec

runSilent :: H.Spec -> IO ()
runSilent = silence . H.hspec

mkAppend :: IO (String -> IO (), IO [String])
mkAppend = do
  ref <- newIORef ([] :: [String])
  let append n = modifyIORef ref (++ [n])
  return (append, readIORef ref)

spec :: Spec
spec = do
  describe "before" $ do
    it "runs an action before every spec item" $ do
      (append, retrieve) <- mkAppend
      runSilent $ H.before (append "before" >> return "value") $ do
        H.it "foo" $ \value -> do
          append (value ++ " foo")
        H.it "bar" $ \value -> do
          append (value ++ " bar")
      retrieve `shouldReturn` ["before", "value foo", "before", "value bar"]

    context "when used multiple times" $ do
      it "is evaluated outside in" $ do
        pending

    context "when used with a QuickCheck property" $ do
      it "runs action before every check of the property" $ do
        (append, retrieve) <- mkAppend
        runSilent $ H.before (append "before" >> return "foo") $ do
          H.it "foo" $ \value -> property $ append value
        retrieve `shouldReturn` (take 200 . cycle) ["before", "foo"]

  describe "before_" $ do
    it "runs an action before every spec item" $ do
      (append, retrieve) <- mkAppend
      runSilent $ H.before_ (append "before") $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      retrieve `shouldReturn` ["before", "foo", "before", "bar"]

    context "when used multiple times" $ do
      it "is evaluated outside in" $ do
        (append, retrieve) <- mkAppend
        runSilent $ H.before_ (append "outer") $  H.before_ (append "inner") $ do
          H.it "foo" $ do
            append "foo"
        retrieve `shouldReturn` ["outer", "inner", "foo"]

    context "when used with a QuickCheck property" $ do
      it "runs action before every check of the property" $ do
        (append, retrieve) <- mkAppend
        runSilent $ H.before_ (append "before") $ do
          H.it "foo" $ property $ append "foo"
        retrieve `shouldReturn` (take 200 . cycle) ["before", "foo"]

  describe "beforeWith" $ do
    it "runs an action before every spec item" $ do
      let action :: Int -> IO String
          action n = return (show n)
      property $ \n -> do
        runSilent $ H.before (return n) $ H.beforeWith action $ do
          H.it "foo" $ (`shouldBe` show n)

  describe "beforeAll" $ do
    it "runs an action before the first spec item" $ do
      (append, retrieve) <- mkAppend
      runSilent $ H.beforeAll (append "beforeAll") $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      retrieve `shouldReturn` [
          "beforeAll"
        , "foo"
        , "bar"
        ]

  describe "beforeAll_" $ do
    it "runs an action before the first spec item" $ do
      (append, retrieve) <- mkAppend
      runSilent $ H.beforeAll (append "beforeAll_") $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      retrieve `shouldReturn` [
          "beforeAll_"
        , "foo"
        , "bar"
        ]

    context "when used with an empty list of examples" $ do
      it "does not run specified action" $ do
        (append, retrieve) <- mkAppend
        runSilent $ H.beforeAll (append "beforeAll") $ do
          return ()
        retrieve `shouldReturn` []

    context "when used with an action that returns a value" $ do
      it "passes that value to the spec item" $ do
        property $ \n -> do
          runSilent $ H.beforeAll (return n) $ do
            H.it "foo" $ \m -> do
              m `shouldBe` (n :: Int)

  describe "after" $ do
    it "must be used with a before action" $ do
      (append, retrieve) <- mkAppend
      runSilent $ H.before (return "from before") $ H.after append $ do
        H.it "foo" $ \_ -> do
          append "foo"
      retrieve `shouldReturn` [
          "foo"
        , "from before"
        ]

  describe "after_" $ do
    it "runs an action after every spec item" $ do
      (append, retrieve) <- mkAppend
      runSilent $ H.after_ (append "after") $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      retrieve `shouldReturn` [
          "foo"
        , "after"
        , "bar"
        , "after"
        ]

    it "guarantees that action is run" $ do
      (append, retrieve) <- mkAppend
      silence . ignoreExitCode $ H.hspec $ H.after_ (append "after") $ do
        H.it "foo" $ do
          ioError $ userError "foo" :: IO ()
          append "foo"
      retrieve `shouldReturn` ["after"]

    context "when used multiple times" $ do
      it "is evaluated inside out" $ do
        (append, retrieve) <- mkAppend
        runSilent $ H.after_ (append "after outer") $  H.after_ (append "after inner") $ do
          H.it "foo" $ do
            append "foo"
        retrieve `shouldReturn` [
            "foo"
          , "after inner"
          , "after outer"
          ]

  describe "afterAll_" $ do
    it "runs an action after the last spec item" $ do
      (append, retrieve) <- mkAppend
      runSilent $ H.afterAll_ (append "afterAll") $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      retrieve `shouldReturn` [
          "foo"
        , "bar"
        , "afterAll"
        ]

    context "when used with an empty list of examples" $ do
      it "does not run specified action" $ do
        (append, retrieve) <- mkAppend
        runSilent $ H.afterAll_ (append "afterAll") $ do
          return ()
        retrieve `shouldReturn` []

    context "when action throws an exception" $ do
      it "reports a failure" $ do
        r <- runSpec $ do
          H.afterAll_ throwException $ do
            H.it "foo" True
        r `shouldSatisfy` any (== "afterAll-hook FAILED [1]")

  describe "around" $ do
    it "wraps every spec item with an action" $ do
      property $ \n -> do
        runSilent $ H.around ($ n) $ do
          H.it "foo" $ \m -> do
            m `shouldBe` (n :: Int)

  describe "around_" $ do
    it "wraps every spec item with an action" $ do
      (append, retrieve) <- mkAppend
      let action e = append "before" >> e >> append "after"
      runSilent $ H.around_ action $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      retrieve `shouldReturn` [
          "before"
        , "foo"
        , "after"
        , "before"
        , "bar"
        , "after"
        ]

    context "when used multiple times" $ do
      it "is evaluated outside in" $ do
        (append, retrieve) <- mkAppend
        let actionOuter e = append "before outer" >> e >> append "after outer"
            actionInner e = append "before inner" >> e >> append "after inner"
        runSilent $ H.around_ actionOuter $ H.around_ actionInner $ do
          H.it "foo" $ do
            append "foo"
        retrieve `shouldReturn` [
            "before outer"
          , "before inner"
          , "foo"
          , "after inner"
          , "after outer"
          ]

  describe "aroundWith" $ do
    it "wraps every spec item with an action" $ do
      let action :: H.ActionWith String -> H.ActionWith Int
          action e n = e (show n)
      property $ \n -> do
        runSilent $ H.before (return n) $ H.aroundWith action $ do
          H.it "foo" $ (`shouldBe` show n)
  where
    runSpec :: H.Spec -> IO [String]
    runSpec = captureLines . H.hspecResult
