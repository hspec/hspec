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
          result = QuickCheckResult 0 "" (QuickCheckOtherFailure "Gave up after 0 tests; 2 discarded!")

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
            , "28"
            ]
        parseQuickCheckResult <$> quickCheckWithResult args {maxSuccess = 2} (verbose p) `shouldReturn`
          QuickCheckResult 2 info (QuickCheckOtherFailure "Passed 2 tests (expected failure).")

    context "with cover" $ do
      context "without checkCoverage" $ do
        let
          p :: Int -> Property
          p n = cover 10 (n == 23) "is 23" True

        it "parses result" $ do
          parseQuickCheckResult <$> qc p `shouldReturn`
            QuickCheckResult 100 "+++ OK, passed 100 tests.\n\nOnly 0% is 23, but expected 10%" QuickCheckSuccess

        it "includes verbose output" $ do
          let
            info = intercalate "\n" [
                "Passed:"
              , "0"
              , ""
              , "Passed:"
              , "28"
              , ""
              , "+++ OK, passed 2 tests."
              , ""
              , "Only 0% is 23, but expected 10%"
              ]
          parseQuickCheckResult <$> quickCheckWithResult args {maxSuccess = 2} (verbose p) `shouldReturn`
            QuickCheckResult 2 info QuickCheckSuccess

      context "with checkCoverage" $ do
        let
          p :: Int -> Property
          p n = checkCoverage $ cover 10 (n == 23) "is 23" True

          failure :: QuickCheckFailure
          failure = QCFailure {
              quickCheckFailureNumShrinks = 0
            , quickCheckFailureException = Nothing
            , quickCheckFailureReason = "Insufficient coverage"
            , quickCheckFailureCounterexample = [
                " 0.8% is 23"
              , ""
              , "Only 0.8% is 23, but expected 10.0%"
              ]
            }

        it "parses result" $ do
          parseQuickCheckResult <$> qc p `shouldReturn`
            QuickCheckResult 400 "" (QuickCheckFailure failure)

        it "includes verbose output" $ do
          let info = intercalate "\n\n" (replicate 399 "Passed:")
          parseQuickCheckResult <$> qc (verbose . p) `shouldReturn`
            QuickCheckResult 400 info (QuickCheckFailure failure)

    context "with Failure" $ do
      context "with single-line failure reason" $ do
        let
          p :: Int -> Bool
          p = (< 1)

          err = "Falsified"
          result = QuickCheckResult 3 "" (QuickCheckFailure $ QCFailure 1 Nothing err ["1"])

        it "parses result" $ do
          parseQuickCheckResult <$> qc p `shouldReturn` result

        it "includes verbose output" $ do
          let info = intercalate "\n" [
                  "Passed:"
                , "0"
                , ""
                , "Passed:"
                , "-1"
                , ""
                , "Failed:"
                , "2"
                , ""
                , "Passed:"
                , "0"
                , ""
                , "Failed:"
                , "1"
                , ""
                , "Passed:"
                , "0"
                ]

          parseQuickCheckResult <$> qc (verbose p) `shouldReturn` result {quickCheckResultInfo = info}

      context "with multi-line failure reason" $ do
        let
          p :: Int -> QCP.Result
          p n = if n /= 2 then QCP.succeeded else QCP.failed {QCP.reason = err}

          err = "foo\nbar"
          result = QuickCheckResult 3 "" (QuickCheckFailure $ QCFailure 0 Nothing err ["2"])

        it "parses result" $ do
          parseQuickCheckResult <$> qc p `shouldReturn` result

        it "includes verbose output" $ do
          let info = intercalate "\n" [
                  "Passed:"
                , "0"
                , ""
                , "Passed:"
                , "-1"
                , ""
                , "Failed:"
                , "2"
                , ""
                , "Passed:"
                , "0"
                , ""
                , "Passed:"
                , "1"
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
