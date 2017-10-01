{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Test.Hspec.Core.QuickCheckUtilSpec (spec) where

import           Helper

import qualified Test.QuickCheck.Property as QCP

import           Test.Hspec.Core.QuickCheckUtil

deriving instance Eq QuickCheckResult
deriving instance Eq Status
deriving instance Eq QuickCheckFailure

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

  describe "stripSuffix" $ do
    it "drops the given suffix from a list" $ do
      stripSuffix "bar" "foobar" `shouldBe` Just "foo"

  describe "splitBy" $ do
    it "splits a string by a given infix" $ do
      splitBy "bar" "foo bar baz" `shouldBe` Just ("foo ", " baz")

  describe "parseQuickCheckResult" $ do
    let
      args = stdArgs {chatty = False, replay = Just (mkGen 0, 0)}
      qc = quickCheckWithResult args

    context "with Success" $ do
      let p :: Int -> Bool
          p n = n == n

      it "parses result" $ do
        parseQuickCheckResult <$> qc p `shouldReturn`
          QuickCheckResult 100 "+++ OK, passed 100 tests." QuickCheckSuccess

      it "includes labels" $ do
        parseQuickCheckResult <$> qc (label "unit" p) `shouldReturn`
          QuickCheckResult 100 "+++ OK, passed 100 tests (100% unit)." QuickCheckSuccess

    context "with GaveUp" $ do
      let p :: Int -> Property
          p n = (n == 1234) ==> True

          qc = quickCheckWithResult args {maxSuccess = 2, maxDiscardRatio = 1}
          result = QuickCheckResult 0 "" (QuickCheckOtherFailure "Gave up after 0 tests!")

      it "parses result" $ do
        parseQuickCheckResult <$> qc p `shouldReturn` result

      it "includes verbose output" $ do
        let
          info = intercalate "\n" [
              "Skipped (precondition false):"
            , "0"
            , ""
            , "Skipped (precondition false):"
            , "0"
            ]
        parseQuickCheckResult <$> qc (verbose p) `shouldReturn` result {quickCheckResultInfo = info}

    context "with NoExpectedFailure" $ do
      let
        p :: Int -> Property
        p _ = expectFailure True

      it "parses result" $ do
        parseQuickCheckResult <$> qc p `shouldReturn`
          QuickCheckResult 100 "" (QuickCheckOtherFailure "Passed 100 tests (expected failure).")

      it "includes verbose output" $ do
        let
          info = intercalate "\n" [
              "Passed:"
            , "0"
            , ""
            , "Passed:"
            , "-39"
            ]
        parseQuickCheckResult <$> quickCheckWithResult args {maxSuccess = 2} (verbose p) `shouldReturn`
          QuickCheckResult 2 info (QuickCheckOtherFailure "Passed 2 tests (expected failure).")

    context "with InsufficientCoverage" $ do
      let
        p :: Int -> Property
        p n = cover (n == 23) 10 "is 23" True

      it "parses result" $ do
        parseQuickCheckResult <$> qc p `shouldReturn`
          QuickCheckResult 100 "" (QuickCheckOtherFailure "Insufficient coverage after 100 tests (only 0% is 23, not 10%).")

      it "includes verbose output" $ do
        let
          info = intercalate "\n" [
              "Passed:"
            , "0"
            , ""
            , "Passed:"
            , "-39"
            ]
        parseQuickCheckResult <$> quickCheckWithResult args {maxSuccess = 2} (verbose p) `shouldReturn`
          QuickCheckResult 2 info (QuickCheckOtherFailure "Insufficient coverage after 2 tests (only 0% is 23, not 10%).")

    context "with Failure" $ do
      context "with single-line failure reason" $ do
        let
          p :: Int -> Bool
          p = (/= 1)

          err = "Falsifiable"
          result = QuickCheckResult 2 "" (QuickCheckFailure $ QCFailure 0 Nothing err ["1"])

        it "parses result" $ do
          parseQuickCheckResult <$> qc p `shouldReturn` result

        it "includes verbose output" $ do
          let info = intercalate "\n" [
                  "Passed:"
                , "0"
                , ""
                , "Failed:"
                , "1"
                , ""
#if MIN_VERSION_QuickCheck(2,11,0)
                , "Passed:"
#else
                , "*** Failed! Passed:"
#endif
                , "0"
                ]

          parseQuickCheckResult <$> qc (verbose p) `shouldReturn` result {quickCheckResultInfo = info}

      context "with multi-line failure reason" $ do
        let
          p :: Int -> QCP.Result
          p n = if n /= 1 then QCP.succeeded else QCP.failed {QCP.reason = err}

          err = "foo\nbar"
          result = QuickCheckResult 2 "" (QuickCheckFailure $ QCFailure 0 Nothing err ["1"])

        it "parses result" $ do
          parseQuickCheckResult <$> qc p `shouldReturn` result

        it "includes verbose output" $ do
          let info = intercalate "\n" [
                  "Passed:"
                , "0"
                , ""
                , "Failed:"
                , "1"
                , ""
#if MIN_VERSION_QuickCheck(2,11,0)
                , "Passed:"
#else
                , "*** Failed! Passed:"
#endif
                , "0"
                ]
          parseQuickCheckResult <$> qc (verbose p) `shouldReturn` result {quickCheckResultInfo = info}

      context "with HUnit assertion" $ do
        let p :: Int -> Int -> Expectation
            p m n = do
              m `shouldBe` n

        it "includes counterexample" $ do
          result <- qc p
          let QuickCheckResult _ _ (QuickCheckFailure r) = parseQuickCheckResult result
          quickCheckFailureCounterexample r `shouldBe` ["0", "1"]
