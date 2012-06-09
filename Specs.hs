{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Main (main) where

import           Test.Hspec.ShouldBe (Specs, describe, it, hspecX, shouldBe)

import qualified Test.Hspec as H
import           Test.Hspec hiding (Specs, describe, it, hspecX)
import           Test.Hspec.Internal (Result(..))
import           Test.Hspec.QuickCheck
import           Test.Hspec.HUnit ()
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
