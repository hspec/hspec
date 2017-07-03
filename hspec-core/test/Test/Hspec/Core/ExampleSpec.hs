{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Hspec.Core.ExampleSpec (spec) where

import           Helper
import           Mock
import           Control.Exception
import           Test.HUnit

import           Test.Hspec.Core.Example (Result(..), Location(..), FailureReason(..))
import qualified Test.Hspec.Core.Example as H
import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Runner as H

safeEvaluateExample :: (H.Example e,  H.Arg e ~ ()) => e -> IO (Either SomeException Result)
safeEvaluateExample e = H.safeEvaluateExample e defaultParams ($ ()) noOpProgressCallback

evaluateExample :: (H.Example e,  H.Arg e ~ ()) => e -> IO Result
evaluateExample e = H.evaluateExample e defaultParams ($ ()) noOpProgressCallback

evaluateExampleWith :: (H.Example e, H.Arg e ~ ()) => (IO () -> IO ()) -> e -> IO Result
evaluateExampleWith action e = H.evaluateExample e defaultParams (action . ($ ())) noOpProgressCallback

evaluateExampleWithArgument :: H.Example e => (ActionWith (H.Arg e) -> IO ()) -> e -> IO Result
evaluateExampleWithArgument action e = H.evaluateExample e defaultParams action noOpProgressCallback

spec :: Spec
spec = do
  describe "safeEvaluateExample" $ do
    context "for Expectation" $ do
      it "returns Failure if an expectation does not hold" $ do
        Right (Failure _ msg) <- safeEvaluateExample (23 `shouldBe` (42 :: Int))
#if MIN_VERSION_HUnit(1,5,0)
        msg `shouldBe` ExpectedButGot Nothing "42" "23"
#else
        msg `shouldBe` Reason "expected: 42\n but got: 23"
#endif

      context "when used with `pending`" $ do
        it "returns Pending" $ do
          Right result <- safeEvaluateExample (H.pending)
          let location =
#if MIN_VERSION_base(4,8,1)
                Just $ Location __FILE__ (__LINE__ - 3) 48
#else
                Nothing
#endif
          result `shouldBe` Pending location Nothing

      context "when used with `pendingWith`" $ do
        it "includes the optional reason" $ do
          Right result <- safeEvaluateExample (H.pendingWith "foo")
          let location =
#if MIN_VERSION_base(4,8,1)
                Just $ Location __FILE__ (__LINE__ - 3) 48
#else
                Nothing
#endif
          result `shouldBe` Pending location (Just "foo")

  describe "evaluateExample" $ do
    context "for Result" $ do
      it "runs around-action" $ do
        ref <- newIORef (0 :: Int)
        let action :: IO () -> IO ()
            action e = do
              e
              modifyIORef ref succ
        evaluateExampleWith action (Failure Nothing NoReason) `shouldReturn` Failure Nothing NoReason
        readIORef ref `shouldReturn` 1

      it "accepts arguments" $ do
        ref <- newIORef (0 :: Int)
        let action :: (Integer -> IO ()) -> IO ()
            action e = do
              e 42
              modifyIORef ref succ
        evaluateExampleWithArgument action (Failure Nothing . Reason . show) `shouldReturn` Failure Nothing (Reason "42")
        readIORef ref `shouldReturn` 1

    context "for Bool" $ do
      it "returns Success on True" $ do
        evaluateExample True `shouldReturn` (Success "")

      it "returns Failure on False" $ do
        evaluateExample False `shouldReturn` Failure Nothing NoReason

      it "propagates exceptions" $ do
        evaluateExample (error "foobar" :: Bool) `shouldThrow` errorCall "foobar"

      it "runs around-action" $ do
        ref <- newIORef (0 :: Int)
        let action :: IO () -> IO ()
            action e = do
              e
              modifyIORef ref succ
        evaluateExampleWith action False `shouldReturn` Failure Nothing NoReason
        readIORef ref `shouldReturn` 1

      it "accepts arguments" $ do
        ref <- newIORef (0 :: Int)
        let action :: (Integer -> IO ()) -> IO ()
            action e = do
              e 42
              modifyIORef ref succ
        evaluateExampleWithArgument action (== (23 :: Integer)) `shouldReturn` Failure Nothing NoReason
        readIORef ref `shouldReturn` 1

    context "for Expectation" $ do
      it "returns Success if all expectations hold" $ do
        evaluateExample (23 `shouldBe` (23 :: Int)) `shouldReturn` (Success "")

      it "propagates exceptions" $ do
        evaluateExample (error "foobar" :: Expectation) `shouldThrow` errorCall "foobar"

      it "runs around-action" $ do
        ref <- newIORef (0 :: Int)
        let action :: IO () -> IO ()
            action e = do
              n <- readIORef ref
              e
              readIORef ref `shouldReturn` succ n
              modifyIORef ref succ
        evaluateExampleWith action (modifyIORef ref succ) `shouldReturn` (Success "")
        readIORef ref `shouldReturn` 2

    context "for Property" $ do
      it "returns Success if property holds" $ do
        evaluateExample (property $ \n -> n == (n :: Int)) `shouldReturn` (Success "")

      it "shows the collected labels" $ do
        Success details <- evaluateExample $ property $ \ () -> label "unit" True
#if MIN_VERSION_QuickCheck(2,10,0)
        details `shouldBe` "100.0% unit\n"
#else
        details `shouldBe` "100% unit\n"
#endif

      it "returns Failure if property does not hold" $ do
        Failure _ _ <- evaluateExample $ property $ \n -> n /= (n :: Int)
        return ()

      it "shows what falsified it" $ do
        Failure _ r <- evaluateExample $ property $ \ (x :: Int) (y :: Int) -> (x == 0 && y == 1) ==> False
        r `shouldBe` (Reason . intercalate "\n")  [
            "Falsifiable (after 1 test):"
          , "  0"
          , "  1"
          ]

      it "runs around-action for each single check of the property" $ do
        ref <- newIORef (0 :: Int)
        let action :: IO () -> IO ()
            action e = do
              n <- readIORef ref
              e
              readIORef ref `shouldReturn` succ n
              modifyIORef ref succ
        Success "" <- evaluateExampleWith action (property $ \(_ :: Int) -> modifyIORef ref succ)
        readIORef ref `shouldReturn` 2000

      it "pretty-prints exceptions" $ do
        Failure _ r <- evaluateExample $ property (\ (x :: Int) -> (x == 0) ==> (throw (ErrorCall "foobar") :: Bool))
        r `shouldBe` (Reason . intercalate "\n") [
            "uncaught exception: ErrorCall"
          , "foobar"
          , "(after 1 test)"
          , "  0"
          ]

      context "when used with Expectation" $ do
        let prop p = property $ \ (x :: Int) (y :: Int) -> (x == 0 && y == 1) ==> p
        context "when used with shouldBe" $ do
          it "shows what falsified it" $ do
            Failure _ err <- evaluateExample $ prop $ 23 `shouldBe` (42 :: Int)
#if MIN_VERSION_HUnit(1,5,0)
            err `shouldBe` ExpectedButGot (Just "Falsifiable (after 1 test):\n  0\n  1") "42" "23"
#else
            err `shouldBe` Reason "Falsifiable (after 1 test):\n  0\n  1\nexpected: 42\n but got: 23"
#endif

        context "when used with assertEqual" $ do
          it "includes prefix" $ do
            Failure _ err <- evaluateExample $ prop $ assertEqual "foobar" (42 :: Int) 23
#if MIN_VERSION_HUnit(1,5,0)
            err `shouldBe` ExpectedButGot (Just "Falsifiable (after 1 test):\n  0\n  1\nfoobar") "42" "23"
#else
            err `shouldBe` Reason "Falsifiable (after 1 test):\n  0\n  1\nfoobar\nexpected: 42\n but got: 23"
#endif

        context "when used with assertFailure" $ do
          it "includes reason" $ do
            Failure _ err <- evaluateExample $ prop (assertFailure "foobar" :: IO ())
            err `shouldBe` Reason "Falsifiable (after 1 test):\n  0\n  1\nfoobar"

      context "when used with `pending`" $ do
        it "returns Pending" $ do
          let location =
#if MIN_VERSION_base(4,8,1)
                Just $ Location __FILE__ (__LINE__ + 4) 37
#else
                Nothing
#endif
          evaluateExample (property H.pending) `shouldReturn` Pending location Nothing

      context "when used with `pendingWith`" $ do
        it "includes the optional reason" $ do
          let location =
#if MIN_VERSION_base(4,8,1)
                Just $ Location __FILE__ (__LINE__ + 4) 39
#else
                Nothing
#endif
          evaluateExample (property $ H.pendingWith "foo") `shouldReturn` Pending location (Just "foo")

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
