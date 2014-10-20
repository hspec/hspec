module Test.Hspec.Core.HooksSpec (main, spec) where

import           Helper
import           Mock
import           Data.IORef

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Runner as H

import qualified Test.Hspec.Core.Hooks as H

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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

  describe "beforeAll" $ do
    it "runs an action before the first spec item" $ do
      ref <- newIORef []
      let append n = modifyIORef ref (++ return n)
      silence $ H.hspec $ H.beforeAll (append "beforeAll") $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      readIORef ref `shouldReturn` [
          "beforeAll"
        , "foo"
        , "bar"
        ]

    context "when used with an empty list of examples" $ do
      it "does not run specified action" $ do
        ref <- newIORef []
        let append n = modifyIORef ref (++ return n)
        silence $ H.hspec $ H.beforeAll (append "beforeAll") $ do
          return ()
        readIORef ref `shouldReturn` []

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

  describe "afterAll" $ do
    it "runs an action after the last spec item" $ do
      ref <- newIORef []
      let append n = modifyIORef ref (++ return n)
      silence $ H.hspec $ H.afterAll (append "afterAll") $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      readIORef ref `shouldReturn` [
          "foo"
        , "bar"
        , "afterAll"
        ]

    context "when used with an empty list of examples" $ do
      it "does not run specified action" $ do
        ref <- newIORef []
        let append n = modifyIORef ref (++ return n)
        silence $ H.hspec $ H.afterAll (append "afterAll") $ do
          return ()
        readIORef ref `shouldReturn` []

    context "when action throws an exception" $ do
      it "reports a failure" $ do
        r <- captureLines . H.hspecResult $ do
          H.afterAll throwException $ do
            H.it "foo" True
        r `shouldSatisfy` any (== "- afterAll-hook FAILED [1]")

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
