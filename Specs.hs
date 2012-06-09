{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Main (main) where

import           Test.Hspec.ShouldBe (Specs, describe, it, hspecX, shouldBe)

import qualified Test.Hspec as H
import           Test.Hspec hiding (Specs, describe, it, hspecX)
import           Test.Hspec.Runner (hHspecWithFormat)
import           Test.Hspec.Internal (SpecTree(..), Spec(..), Result(..))
import           Test.Hspec.Formatters
import           Test.Hspec.QuickCheck
import           Test.Hspec.HUnit ()
import           Test.HUnit
import           System.IO
import           System.IO.Silently
import           Data.List (isPrefixOf)
import qualified Test.HUnit as HUnit

main :: IO ()
main = specs >>= hspecX

specs :: IO Specs
specs = do
  let testSpecs = [H.describe "Example" [
          H.it "success" (Success),
          H.it "fail 1" (Fail "fail message"),
          H.it "pending" (pending "pending message"),
          H.it "fail 2" (HUnit.assertEqual "assertEqual test" 1 (2::Int)),
          H.it "exceptions" (undefined :: Bool),
          H.it "quickcheck" (property $ \ i -> i == (i+1::Integer))]
          ]

  (reportContents, exampleSpecs)     <- capture $ hHspecWithFormat specdoc         False stdout testSpecs

  let report = lines reportContents

  return $ do
    describe "the \"describe\" function" $ do
        it "takes a description of what the behavior is for" $
            case exampleSpecs of
              [SpecGroup "Example" _] -> True
              _ -> False

        it "groups behaviors for what's being described" $
            case exampleSpecs of
              [SpecGroup _ xs] -> length xs == 6
              _ -> False

        describe "a nested description" $ do
            it "has it's own specs"
                (True)

    describe "the \"it\" function" $ do
        it "takes a description of a desired behavior" $
            case unSpec $ H.it "whatever" Success of
              SpecExample requirement _ -> requirement == "whatever"
              _ -> False

        it "takes an example of that behavior" $ do
            case unSpec $ H.it "whatever" Success of
              SpecExample _ example -> do
                r <- example
                r @?= Success
              SpecGroup _ _ -> assertFailure "unexpected SpecGroup"

        it "can use a Bool, HUnit Test, QuickCheck property, or \"pending\" as an example"
            (True)

        it "will treat exceptions as failures"
            (any (==" - exceptions FAILED [3]") report)

    describe "the \"hspec\" function" $ do
        it "displays a header for each thing being described"
            (any (=="Example") report)

        it "displays one row for each behavior" $ do
            length report `shouldBe` 29

        it "displays a row for each successfull, failed, or pending example"
            (any (==" - success") report && any (==" - fail 1 FAILED [1]") report)

        it "displays a detailed list of failed examples"
            (any (=="1) Example fail 1 FAILED") report)

        it "displays a '#' with an additional message for pending examples"
            (any (=="     # PENDING: pending message") report )

        it "summarizes the time it takes to finish"
            (any ("Finished in " `isPrefixOf`) report)

        it "summarizes the number of examples and failures"
            (any (=="6 examples, 4 failures") report)

        it "outputs failed examples in red, pending in yellow, and successful in green"
            (True)

    describe "Bool as an example" $ do
        it "is just an expression that evaluates to a Bool"
            (True)

    describe "HUnit TestCase as an example" $ do
        it "is specified with the HUnit \"TestCase\" data constructor"
            (HUnit.TestCase $ HUnit.assertBool "example" True)

        it "is the assumed example for IO() actions"
            (HUnit.assertBool "example" True)

        it "will show the failed assertion text if available (e.g. assertBool)" $ do
          (innerReport, _) <- capture $ hHspec stdout $ [H.describe "" [ H.it "" (HUnit.assertBool "trivial" False)]]
          HUnit.assertBool "should find assertion text" $ any (=="trivial") (lines innerReport)

        it "will show the failed assertion expected and actual values if available (e.g. assertEqual)" $ do
          (innerReportContents, _) <- capture $ hHspec stdout $ [H.describe "" [ H.it "" (HUnit.assertEqual "trivial" (1::Int) 2)]]
          let innerReport = lines innerReportContents
          HUnit.assertBool "should find assertion text" $ any (=="trivial") innerReport
          HUnit.assertBool "should find 'expected: 1'" $ any (=="expected: 1") innerReport
          HUnit.assertBool "should find ' but got: 2'" $ any (==" but got: 2") innerReport

    describe "QuickCheck property as an example" $ do
        it "is specified with the \"property\" function"
            (property $ \ b -> b || True)

        it "will show what falsified it"
            (any (=="0") report)

    describe "pending as an example" $ do
        it "is specified with the \"pending\" function and an explanation" True

        it "accepts a message to display in the report" True

    describe "the \"hHspecWithFormat\" function" $ do
        it "can use the \"silent\" formatter to show no output" $ do
            (r, _) <- capture $ hHspecWithFormat silent False stdout testSpecs
            r `shouldBe` ""

        it "can use the \"progress\" formatter to show '..F...FF.F' style output" $ do
            (r, _) <- capture $ hHspecWithFormat progress False stdout testSpecs
            head (lines r) `shouldBe` ".F.FFF"

        it "can use the \"specdoc\" formatter to show all examples (default)" $ do
            (lines reportContents !! 1) `shouldBe` "Example"

        it "can use the \"failed_examples\" formatter to show only failed examples" $ do
            (r, _) <- capture $ hHspecWithFormat failed_examples False stdout testSpecs
            (lines r !! 1) `shouldBe` "1) Example fail 1 FAILED"
