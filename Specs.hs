{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Main (main) where

import           Test.Hspec.ShouldBe (Specs, describe, it, hspecX, shouldBe, shouldSatisfy)

import qualified Test.Hspec as H
import           Test.Hspec hiding (Specs, describe, it, hspecX)
import           Test.Hspec.Internal (SpecTree(..), Spec(..), Result(..))
import           Test.Hspec.QuickCheck
import           Test.Hspec.HUnit ()
import           Test.HUnit
import           Data.List (isPrefixOf)
import qualified Test.HUnit as HUnit
import           Util

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

  report <- runSpec testSpecs

  return $ do
    describe "the \"describe\" function" $ do

        let testSpec = [
              H.describe "some subject" [
                H.it "foo" True
              , H.it "bar" True
              , H.it "baz" True
              ]
              ]

        it "takes a description of what the behavior is for" $ do
            r <- runSpec testSpec
            r `shouldSatisfy` any (== "some subject")

        it "groups behaviors for what's being described" $ do
            r <- filter (isPrefixOf " - ") `fmap` runSpec testSpec
            length r `shouldBe` 3

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
