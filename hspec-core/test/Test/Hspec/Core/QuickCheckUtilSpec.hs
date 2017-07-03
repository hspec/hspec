{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Test.Hspec.Core.QuickCheckUtilSpec (spec) where

import           Helper

import           Control.Exception

import           Test.Hspec.Core.QuickCheckUtil

spec :: Spec
spec = do
  describe "formatNumbers" $ do
    it "includes number of tests" $ do
      formatNumbers 1 0 `shouldBe` "(after 1 test)"

    it "pluralizes number of tests" $ do
      formatNumbers 3 0 `shouldBe` "(after 3 tests)"

    it "includes number of shrinks" $ do
      formatNumbers 3 1 `shouldBe` "(after 3 tests and 1 shrink)"

    it "pluralizes number of shrinks" $ do
      formatNumbers 3 3 `shouldBe` "(after 3 tests and 3 shrinks)"

  describe "parseQuickCheckResult" $ do
    let qc = quickCheckWithResult stdArgs {chatty = False, replay = Just (mkGen 0, 0)}

    context "with Success" $ do
      let p :: Int -> Property
          p n = (label "unit" $ n == n)

      it "includes test count" $ do
        result <- qc p
        let QuickCheckResult n (QuickCheckSuccess _) = parseQuickCheckResult result
        n `shouldBe` 100

      it "includes labels" $ do
        result <- qc p
        let QuickCheckResult _ (QuickCheckSuccess s) = parseQuickCheckResult result
        s `shouldBe` "100% unit\n"

    context "with GaveUp" $ do
      let p :: Int -> Property
          p n = (n == 23) ==> True

      it "includes reason" $ do
        result <- qc p
        let QuickCheckResult _ (QuickCheckOtherFailure r) = parseQuickCheckResult result
        r `shouldBe` "Gave up after 3 tests"

    context "with NoExpectedFailure" $ do
      let p :: Int -> Property
          p _ = expectFailure True

      it "includes reason" $ do
        result <- qc p
        let QuickCheckResult _ (QuickCheckOtherFailure r) = parseQuickCheckResult result
        r `shouldBe` "Passed 100 tests (expected failure)"

    context "with InsufficientCoverage" $ do
      let p :: Int -> Property
          p n = cover (n == 23) 10 "is 23" True

      it "includes reason" $ do
        result <- qc p
        let QuickCheckResult _ (QuickCheckOtherFailure r) = parseQuickCheckResult result
        r `shouldBe` "Insufficient coverage after 100 tests (only 0% is 23, not 10%)."

    context "with Failure" $ do
      context "with single-line failure reason" $ do
        let p :: Int -> Int -> Bool
            p _ _ = False

        it "includes counterexample" $ do
          result <- qc p
          let QuickCheckResult _ (QuickCheckFailure r) = parseQuickCheckResult result
          quickCheckFailureCounterexample r `shouldBe` "0\n0"

        it "includes reason" $ do
          result <- qc p
          let QuickCheckResult _ (QuickCheckFailure r) = parseQuickCheckResult result
          quickCheckFailureReason r `shouldBe` "Falsifiable"

      context "with multi-line failure reason" $ do
        let p :: Int -> Int -> IO ()
            p _ _ = throwIO (ErrorCall "foo\nbar")

        it "includes counterexample" $ do
          result <- qc p
          let QuickCheckResult _ (QuickCheckFailure r) = parseQuickCheckResult result
          quickCheckFailureCounterexample r `shouldBe` "0\n0"

        it "includes reason" $ do
          result <- qc p
          let QuickCheckResult _ (QuickCheckFailure r) = parseQuickCheckResult result
          (take 3 . lines . quickCheckFailureReason) r `shouldBe` [
              "Exception:"
            , "  foo"
            , "  bar"
            ]

      context "with HUnit assertion" $ do
        let p :: Int -> Int -> Expectation
            p m n = do
              m `shouldBe` n

        it "includes counterexample" $ do
          result <- qc p
#if MIN_VERSION_QuickCheck(2,10,0)
          let QuickCheckResult _ (QuickCheckFailure r) = parseQuickCheckResult result
          quickCheckFailureCounterexample r `shouldBe` "0\n1"
#else
          let QuickCheckResult _ (QuickCheckOtherFailure err) = parseQuickCheckResult result
          err `shouldBe` "Failed! (after 2 tests and 2 shrinks): \nexpected: 1\n but got: 0\n0\n1"
#endif
