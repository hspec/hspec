{-# LANGUAGE CPP, ScopedTypeVariables, TypeFamilies #-}
module Test.Hspec.Core.ExampleSpec (main, spec) where

import           Helper
import           Mock
import           Data.List

import qualified Test.Hspec.Core.Example as H
import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Runner as H

main :: IO ()
main = hspec spec

evaluateExample :: (H.Example e,  H.Arg e ~ ()) => e -> IO H.Result
evaluateExample e = H.evaluateExample e defaultParams ($ ()) noOpProgressCallback

evaluateExampleWith :: (H.Example e, H.Arg e ~ ()) => (IO () -> IO ()) -> e -> IO H.Result
evaluateExampleWith action e = H.evaluateExample e defaultParams (action . ($ ())) noOpProgressCallback

spec :: Spec
spec = do
  describe "evaluateExample" $ do
    context "for Bool" $ do
      it "returns Success on True" $ do
        evaluateExample True `shouldReturn` H.Success

      it "returns Fail on False" $ do
        evaluateExample False `shouldReturn` H.Fail ""

      it "propagates exceptions" $ do
        pending
        evaluateExample (error "foobar" :: Bool) `shouldThrow` errorCall "foobar"

    context "for Expectation" $ do
      it "returns Success if all expectations hold" $ do
        evaluateExample (23 `shouldBe` (23 :: Int)) `shouldReturn` H.Success

      it "returns Fail if an expectation does not hold" $ do
        evaluateExample (23 `shouldBe` (42 :: Int)) `shouldReturn` H.Fail "expected: 42\n but got: 23"

      it "propagates exceptions" $ do
        evaluateExample (error "foobar" :: Expectation) `shouldThrow` errorCall "foobar"

      it "runs provided action around expectation" $ do
        ref <- newIORef (0 :: Int)
        let action :: IO () -> IO ()
            action e = do
              n <- readIORef ref
              e
              readIORef ref `shouldReturn` succ n
              modifyIORef ref succ
        evaluateExampleWith action (modifyIORef ref succ) `shouldReturn` H.Success
        readIORef ref `shouldReturn` 2

      context "when used with `pending`" $ do
        it "returns Pending" $ do
          evaluateExample (H.pending) `shouldReturn` H.Pending Nothing

      context "when used with `pendingWith`" $ do
        it "includes the optional reason" $ do
          evaluateExample (H.pendingWith "foo") `shouldReturn` H.Pending (Just "foo")

    context "for Property" $ do
      it "returns Success if property holds" $ do
        evaluateExample (property $ \n -> n == (n :: Int)) `shouldReturn` H.Success

      it "returns Fail if property does not hold" $ do
        H.Fail _ <- evaluateExample $ property $ \n -> n /= (n :: Int)
        return ()

      it "shows what falsified it" $ do
        H.Fail r <- evaluateExample $ property $ \ (x :: Int) (y :: Int) -> (x == 0 && y == 1) ==> False
        r `shouldBe` intercalate "\n"  [
            "Falsifiable (after 1 test): "
          , "0"
          , "1"
          ]

      it "runs provided action around each single check of the property" $ do
        ref <- newIORef (0 :: Int)
        let action :: IO () -> IO ()
            action e = do
              n <- readIORef ref
              e
              readIORef ref `shouldReturn` succ n
              modifyIORef ref succ
        H.Success <- evaluateExampleWith action (property $ modifyIORef ref succ)
        readIORef ref `shouldReturn` 2000

      it "pretty-prints exceptions" $ do
        H.Fail r <- evaluateExample $ property (\ (x :: Int) -> (x == 0) ==> (error "foobar" :: Bool))
        r `shouldBe` intercalate "\n" [
#if MIN_VERSION_QuickCheck(2,7,0)
            "uncaught exception: ErrorCall (foobar) (after 1 test)"
#else
            "Exception: 'foobar' (after 1 test): "
#endif
          , "0"
          ]

      context "when used with shouldBe" $ do
        it "shows what falsified it" $ do
          H.Fail r <- evaluateExample $ property $ \ (x :: Int) (y :: Int) -> (x == 0 && y == 1) ==> 23 `shouldBe` (42 :: Int)
          r `shouldBe` intercalate "\n" [
              "Falsifiable (after 1 test): "
            , "expected: 42"
            , " but got: 23"
            , "0"
            , "1"
            ]

      context "when used with `pending`" $ do
        it "returns Pending" $ do
          evaluateExample (property H.pending) `shouldReturn` H.Pending Nothing

      context "when used with `pendingWith`" $ do
        it "includes the optional reason" $ do
          evaluateExample (property $ H.pendingWith "foo") `shouldReturn` H.Pending (Just "foo")

  describe "Expectation" $ do
    context "as a QuickCheck property" $ do
      it "can be quantified" $ do
        e <- newMock
        silence . H.hspec $ do
          H.it "some behavior" $ property $ \xs -> do
            mockAction e
            (reverse . reverse) xs `shouldBe` (xs :: [Int])
        mockCounter e `shouldReturn` 100

      it "can be used with expectations/HUnit assertions" $ do
        silence . H.hspecResult $ do
          H.describe "readIO" $ do
            H.it "is inverse to show" $ property $ \x -> do
              (readIO . show) x `shouldReturn` (x :: Int)
        `shouldReturn` H.Summary 1 0
