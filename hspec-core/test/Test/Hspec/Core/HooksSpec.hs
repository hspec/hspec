{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.Core.HooksSpec (spec) where

import           Control.Exception
import           Helper
import           Prelude ()

import qualified Test.Hspec.Core.Runner as H
import qualified Test.Hspec.Core.Spec as H

import qualified Test.Hspec.Core.Hooks as H

runSilent :: H.Spec -> IO ()
runSilent = silence . H.hspec

mkAppend :: IO (String -> IO (), IO [String])
mkAppend = do
  ref <- newIORef ([] :: [String])
  let rec n = modifyIORef ref (++ [n])
  return (rec, readIORef ref)

spec :: Spec
spec = do
  describe "before" $ do
    it "runs an action before every spec item" $ do
      (rec, retrieve) <- mkAppend
      runSilent $ H.before (rec "before" >> return "value") $ do
        H.it "foo" $ \value -> do
          rec (value ++ " foo")
        H.it "bar" $ \value -> do
          rec (value ++ " bar")
      retrieve `shouldReturn` ["before", "value foo", "before", "value bar"]

    context "when used with a QuickCheck property" $ do
      it "runs action before every check of the property" $ do
        (rec, retrieve) <- mkAppend
        runSilent $ H.before (rec "before" >> return "value") $ do
          H.it "foo" $ \value -> property $ \(_ :: Int) -> rec value
        retrieve `shouldReturn` (take 200 . cycle) ["before", "value"]

  describe "before_" $ do
    it "runs an action before every spec item" $ do
      (rec, retrieve) <- mkAppend
      runSilent $ H.before_ (rec "before") $ do
        H.it "foo" $ do
          rec "foo"
        H.it "bar" $ do
          rec "bar"
      retrieve `shouldReturn` ["before", "foo", "before", "bar"]

    context "when used multiple times" $ do
      it "is evaluated outside in" $ do
        (rec, retrieve) <- mkAppend
        runSilent $ H.before_ (rec "outer") $ H.before_ (rec "inner") $ do
          H.it "foo" $ do
            rec "foo"
        retrieve `shouldReturn` ["outer", "inner", "foo"]

    context "when used with a QuickCheck property" $ do
      it "runs action before every check of the property" $ do
        (rec, retrieve) <- mkAppend
        runSilent $ H.before_ (rec "before") $ do
          H.it "foo" $ property $ \(_ :: Int) -> rec "foo"
        retrieve `shouldReturn` (take 200 . cycle) ["before", "foo"]

      context "when used multiple times" $ do
        it "is evaluated outside in" $ do
          (rec, retrieve) <- mkAppend
          runSilent $ H.before_ (rec "outer") $ H.before_ (rec "inner") $ do
            H.it "foo" $ property $ \(_ :: Int) -> rec "foo"
          retrieve `shouldReturn` (take 300 . cycle) ["outer", "inner", "foo"]

  describe "beforeWith" $ do
    it "transforms spec argument" $ do
      (rec, retrieve) <- mkAppend
      let action :: Int -> IO String
          action = return . show
      runSilent $ H.before (return 23) $ H.beforeWith action $ do
        H.it "foo" $ \value -> rec value
      retrieve `shouldReturn` ["23"]

    it "can be used multiple times" $ do
      let action1 :: Int -> IO Int
          action1 = return . succ

          action2 :: Int -> IO String
          action2 = return . show

          action3 :: String -> IO String
          action3 = return . ("foo " ++)

      (rec, retrieve) <- mkAppend

      runSilent $ H.before (return 23) $ H.beforeWith action1 $ H.beforeWith action2 $ H.beforeWith action3 $ do
        H.it "foo" $ \value -> rec value

      retrieve `shouldReturn` ["foo 24"]

  describe "beforeAll" $ do
    it "runs an action before the first spec item" $ do
      (rec, retrieve) <- mkAppend
      runSilent $ H.beforeAll (rec "beforeAll" >> return "value") $ do
        H.it "foo" $ \value -> do
          rec $ "foo " ++ value
        H.it "bar" $ \value -> do
          rec $ "bar " ++ value
      retrieve `shouldReturn` [
          "beforeAll"
        , "foo value"
        , "bar value"
        ]

    context "when specified action throws an exception" $ do
      it "sets subsequent spec items to pending" $ do
        result <- silence . H.hspecResult $ H.beforeAll (throwIO (ErrorCall "foo")) $ do
          H.it "foo" $ \n -> do
            n `shouldBe` (23 :: Int)
          H.it "bar" $ \n -> do
            n `shouldBe` 23
        result `shouldBe` H.Summary {H.summaryExamples = 2, H.summaryFailures = 1}

    context "when used with an empty list of examples" $ do
      it "does not run specified action" $ do
        (rec, retrieve) <- mkAppend
        runSilent $ H.beforeAll (rec "beforeAll" >> return "value") $ do
          return ()
        retrieve `shouldReturn` []

  describe "beforeAll_" $ do
    it "runs an action before the first spec item" $ do
      (rec, retrieve) <- mkAppend
      runSilent $ H.beforeAll_ (rec "beforeAll_") $ do
        H.it "foo" $ do
          rec "foo"
        H.it "bar" $ do
          rec "bar"
      retrieve `shouldReturn` [
          "beforeAll_"
        , "foo"
        , "bar"
        ]

    context "when used multiple times" $ do
      it "is evaluated outside in" $ do
        (rec, retrieve) <- mkAppend
        runSilent $ H.beforeAll_ (rec "outer") $ H.beforeAll_ (rec "inner") $ do
          H.it "foo" $ do
            rec "foo"
          H.it "bar" $ do
            rec "bar"
        retrieve `shouldReturn` [
            "outer"
          , "inner"
          , "foo"
          , "bar"
          ]

  describe "after" $ do
    it "runs an action after every spec item" $ do
      (rec, retrieve) <- mkAppend
      runSilent $ H.before (rec "before" >> return "from before") $ H.after rec $ do
        H.it "foo" $ \_ -> do
          rec "foo"
        H.it "bar" $ \_ -> do
          rec "bar"
      retrieve `shouldReturn` [
          "before"
        , "foo"
        , "from before"
        , "before"
        , "bar"
        , "from before"
        ]

    it "guarantees that action is run" $ do
      (rec, retrieve) <- mkAppend
      silence . ignoreExitCode . H.hspec $ H.before (rec "before" >> return "from before") $ H.after rec $ do
        H.it "foo" $ \_ -> do
          ioError $ userError "foo" :: IO ()
          rec "foo"
      retrieve `shouldReturn` ["before", "from before"]

  describe "after_" $ do
    it "runs an action after every spec item" $ do
      (rec, retrieve) <- mkAppend
      runSilent $ H.after_ (rec "after") $ do
        H.it "foo" $ do
          rec "foo"
        H.it "bar" $ do
          rec "bar"
      retrieve `shouldReturn` [
          "foo"
        , "after"
        , "bar"
        , "after"
        ]

    it "guarantees that action is run" $ do
      (rec, retrieve) <- mkAppend
      silence . ignoreExitCode $ H.hspec $ H.after_ (rec "after") $ do
        H.it "foo" $ do
          ioError $ userError "foo" :: IO ()
          rec "foo"
      retrieve `shouldReturn` ["after"]

    context "when used multiple times" $ do
      it "is evaluated inside out" $ do
        (rec, retrieve) <- mkAppend
        runSilent $ H.after_ (rec "after outer") $ H.after_ (rec "after inner") $ do
          H.it "foo" $ do
            rec "foo"
        retrieve `shouldReturn` [
            "foo"
          , "after inner"
          , "after outer"
          ]

  describe "afterAll" $ do
    it "runs an action after the last spec item" $ do
      (rec, retrieve) <- mkAppend
      runSilent $ H.before (rec "before" >> return "from before") $ H.afterAll rec $ do
        H.it "foo" $ \_ -> do
          rec "foo"
        H.it "bar" $ \_ -> do
          rec "bar"
      retrieve `shouldReturn` [
          "before"
        , "foo"
        , "before"
        , "bar"
        , "before"
        , "from before"
        ]

    context "when used with an empty list of examples" $ do
      it "does not run specified action" $ do
        (rec, retrieve) <- mkAppend
        runSilent $ H.before (rec "before" >> return "from before") $ H.afterAll rec $ do
          return ()
        retrieve `shouldReturn` []

    context "when action throws an exception" $ do
      it "reports a failure" $ do
        r <- runSpec $ H.before (return "from before") $ H.afterAll (\_ -> throwException) $ do
          H.it "foo" $ \a -> a `shouldBe` "from before"
        r `shouldSatisfy` any (== "afterAll-hook FAILED [1]")

  describe "afterAll_" $ do
    it "runs an action after the last spec item" $ do
      (rec, retrieve) <- mkAppend
      runSilent $ H.before_ (rec "before") $ H.afterAll_ (rec "afterAll_") $ do
        H.it "foo" $ do
          rec "foo"
        H.it "bar" $ do
          rec "bar"
      retrieve `shouldReturn` [
          "before"
        , "foo"
        , "before"
        , "bar"
        , "before"
        , "afterAll_"
        ]

    context "when used multiple times" $ do
      it "is evaluated inside out" $ do
        (rec, retrieve) <- mkAppend
        runSilent $ H.afterAll_ (rec "after outer") $ H.afterAll_ (rec "after inner") $ do
          H.it "foo" $ do
            rec "foo"
        retrieve `shouldReturn` [
            "foo"
          , "after inner"
          , "after outer"
          ]

    context "when used with an empty list of examples" $ do
      it "does not run specified action" $ do
        (rec, retrieve) <- mkAppend
        runSilent $ H.afterAll_ (rec "afterAll_") $ do
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
      (rec, retrieve) <- mkAppend
      let action e = rec "before" >> e "from around" >> rec "after"
      runSilent $ H.around action $ do
        H.it "foo" $ rec . ("foo " ++)
        H.it "bar" $ rec . ("bar " ++)
      retrieve `shouldReturn` [
          "before"
        , "foo from around"
        , "after"
        , "before"
        , "bar from around"
        , "after"
        ]

  describe "around_" $ do
    it "wraps every spec item with an action" $ do
      (rec, retrieve) <- mkAppend
      let action e = rec "before" >> e >> rec "after"
      runSilent $ H.around_ action $ do
        H.it "foo" $ do
          rec "foo"
        H.it "bar" $ do
          rec "bar"
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
        (rec, retrieve) <- mkAppend
        let actionOuter e = rec "before outer" >> e >> rec "after outer"
            actionInner e = rec "before inner" >> e >> rec "after inner"
        runSilent $ H.around_ actionOuter $ H.around_ actionInner $ do
          H.it "foo" $ do
            rec "foo"
        retrieve `shouldReturn` [
            "before outer"
          , "before inner"
          , "foo"
          , "after inner"
          , "after outer"
          ]

  describe "aroundWith" $ do
    it "wraps every spec item with an action" $ do
      (rec, retrieve) <- mkAppend
      let action :: H.ActionWith String -> H.ActionWith Int
          action e = e . show
      runSilent $ H.before (return 23) $ H.aroundWith action $ do
        H.it "foo" rec
      retrieve `shouldReturn` ["23"]
  where
    runSpec :: H.Spec -> IO [String]
    runSpec = captureLines . H.hspecResult
