module Test.HspecSpec (main, spec) where

import           Helper
import           Mock
import           Data.IORef
import           Data.List (isPrefixOf)

import           Test.Hspec.Core (SpecTree(..), Item(..), Result(..), runSpecM)
import qualified Test.Hspec as H
import qualified Test.Hspec.Runner as H (hspecResult)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pending" $ do
    it "specifies a pending example" $ do
      r <- runSpec $ do
        H.it "foo" H.pending
      r `shouldSatisfy` any (== "     # PENDING: No reason given")

  describe "pendingWith" $ do
    it "specifies a pending example with a reason for why it's pending" $ do
      r <- runSpec $ do
        H.it "foo" $ do
          H.pendingWith "for some reason"
      r `shouldSatisfy` any (== "     # PENDING: for some reason")

  describe "describe" $ do
    let testSpec = do
          H.describe "some subject" $ do
            H.it "foo" True
            H.it "bar" True
            H.it "baz" True
    it "takes a description of what the behavior is for" $ do
      r <- runSpec testSpec
      r `shouldSatisfy` any (== "some subject")

    it "groups behaviors for what's being described" $ do
      r <- filter (isPrefixOf "  - ") `fmap` runSpec testSpec
      length r `shouldBe` 3

    it "can be nested" $ do
      let [SpecGroup foo [SpecGroup bar [SpecItem baz _]]] = runSpecM $ do
            H.describe "foo" $ do
              H.describe "bar" $ do
                H.it "baz" True
      (foo, bar, baz) `shouldBe` ("foo", "bar", "baz")

    context "when no description is given" $ do
      it "uses a default description" $ do
        let [SpecGroup d _] = runSpecM (H.describe "" (pure ()))
        d `shouldBe` "(no description given)"

  describe "it" $ do
    it "takes a description of a desired behavior" $ do
      let [SpecItem requirement _] = runSpecM (H.it "whatever" True)
      requirement `shouldBe` "whatever"

    it "takes an example of that behavior" $ do
      let [SpecItem _ item] = runSpecM (H.it "whatever" True)
      itemExample item defaultParams id `shouldReturn` Success

    context "when no description is given" $ do
      it "uses a default description" $ do
        let [SpecItem requirement _] = runSpecM (H.it "" True)
        requirement `shouldBe` "(unspecified behavior)"

  describe "example" $ do
    it "fixes the type of an expectation" $ do
      r <- runSpec $ do
        H.it "foo" $ H.example $ do
          pure ()
      r `shouldSatisfy` any (== "1 example, 0 failures")

  describe "parallel" $ do
    it "marks examples for parallel execution" $ do
      let [SpecItem _ item] = runSpecM . H.parallel $ H.it "whatever" True
      itemIsParallelizable item `shouldBe` True

    it "is applied recursively" $ do
      let [SpecGroup _ [SpecGroup _ [SpecItem _ item]]] = runSpecM . H.parallel $ do
            H.describe "foo" $ do
              H.describe "bar" $ do
                H.it "baz" True
      itemIsParallelizable item `shouldBe` True

  describe "before" $ do
    it "runs an action before each spec item" $ do
      mock <- newMock
      silence $ H.hspec $ H.before (mockAction mock) $ do
        H.it "foo" $ do
          mockCounter mock `shouldReturn` 1
        H.it "bar" $ do
          mockCounter mock `shouldReturn` 2
      mockCounter mock `shouldReturn` 2

    context "when used multiple times" $ do
      it "is evaluated outside in" $ do
        ref <- newIORef (0 :: Int)
        let action1 = do
              readIORef ref `shouldReturn` 0
              modifyIORef ref succ
            action2 = do
              readIORef ref `shouldReturn` 1
              modifyIORef ref succ
        silence $ H.hspec $ H.before action1 $ H.before action2 $ do
          H.it "foo" $ do
            readIORef ref `shouldReturn` 2

  describe "after" $ do
    it "runs an action after each spec item" $ do
      mock <- newMock
      silence $ H.hspec $ H.after (mockAction mock) $ do
        H.it "foo" $ do
          mockCounter mock `shouldReturn` 0
        H.it "bar" $ do
          mockCounter mock `shouldReturn` 1
      mockCounter mock `shouldReturn` 2

    it "guarantees that action is run" $ do
      mock <- newMock
      silence . ignoreExitCode $ H.hspec $ H.after (mockAction mock) $ do
        H.it "foo" $ do
          ioError $ userError "foo" :: IO ()
      mockCounter mock `shouldReturn` 1

  describe "around" $ do
    it "wraps each spec item with an action" $ do
      ref <- newIORef (0 :: Int)
      let action :: IO () -> IO ()
          action e = do
            readIORef ref `shouldReturn` 0
            writeIORef ref 1
            e
            readIORef ref `shouldReturn` 2
            writeIORef ref 3
      silence $ H.hspec $ H.around action $ do
        H.it "foo" $ do
          readIORef ref `shouldReturn` 1
          writeIORef ref 2
      readIORef ref `shouldReturn` 3
  where
    runSpec :: H.Spec -> IO [String]
    runSpec = captureLines . H.hspecResult
