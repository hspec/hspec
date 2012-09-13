module Test.Hspec.FormattersSpec (main, spec) where

import           Test.Hspec.Meta
import           Data.List

import           Util
import           System.Console.ANSI

import qualified Test.Hspec.Core as H
import qualified Test.Hspec.Runner as H

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "specdoc" $ do
    let testSpecs = [H.describe "Example" [
            H.it "success"    (H.Success)
          , H.it "fail 1"     (H.Fail "fail message")
          , H.it "pending"    (H.pending "pending message")
          , H.it "fail 2"     (H.Fail "")
          , H.it "exceptions" (undefined :: H.Result)
          , H.it "fail 3"     (H.Fail "")
          ]
          ]

    it "displays a header for each thing being described" $ do
      r <- runSpec testSpecs
      r `shouldSatisfy` any (== "Example")

    it "displays one row for each behavior" $ do
      pending

    it "displays a row for each successfull, failed, or pending example" $ do
      r <- runSpec testSpecs
      r `shouldSatisfy` any (== " - fail 1 FAILED [1]")
      r `shouldSatisfy` any (== " - success")

    it "displays a detailed list of failed examples" $ do
      r <- runSpec testSpecs
      r `shouldSatisfy` any (== "1) Example fail 1 FAILED")

    it "displays a '#' with an additional message for pending examples" $ do
      r <- runSpec testSpecs
      r `shouldSatisfy` any (== "     # PENDING: pending message")

    it "summarizes the time it takes to finish" $ do
      r <- runSpec testSpecs
      r `shouldSatisfy` any ("Finished in " `isPrefixOf`)

    it "outputs failed examples in red, pending in yellow, and successful in green" $ do
      pending

    it "marks examples that throw exceptions, includes the exception type" $ do
      r <- runSpec [H.it "foobar" (undefined :: Bool)]
      r `shouldSatisfy` isInfixOf [
          "1) foobar FAILED (uncaught exception)"
        , "ErrorCall (Prelude.undefined)"
        ]

    it "summarizes the number of examples and failures" $ do
      r <- runSpec testSpecs
      r `shouldSatisfy` any (== "6 examples, 4 failures")

    it "shows summary in green if there are no failures" $ do
      r <- capture $ H.hspecWith H.defaultConfig {H.configColorMode = H.ColorAlway} [H.it "foobar" True]
      r `shouldSatisfy` any (== (green ++ "1 example, 0 failures" ++ reset))

    it "shows summary in red if there are failures" $ do
      r <- capture $ H.hspecWith H.defaultConfig {H.configColorMode = H.ColorAlway} [H.it "foobar" False]
      r `shouldSatisfy` any (== (red ++ "1 example, 1 failure" ++ reset))
  where
    green  = setSGRCode [SetColor Foreground Dull Green]
    red    = setSGRCode [SetColor Foreground Dull Red]
    reset  = setSGRCode [Reset]
