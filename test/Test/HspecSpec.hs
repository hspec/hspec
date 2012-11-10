module Test.HspecSpec (main, spec) where

import           Test.Hspec.Meta
import           Util (capture__)
import           Data.List (isPrefixOf)

import qualified Test.Hspec.Core.Type as H (defaultParams)
import           Test.Hspec.Core (SpecTree(..), Result(..), runSpecM)
import qualified Test.Hspec as H
import qualified Test.Hspec.Runner as H (hspecWith)
import           Test.Hspec.Runner (defaultConfig)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pending" $ do
    it "specifies a pending example" $ do
      r <- runSpec $ do
        H.it "foo" H.pending
      r `shouldSatisfy` any (== "     # PENDING: No reason given")

    it "accepts an optional message, which is included in the report" $ do
      r <- runSpec $ do
        H.it "foo" $ do
          H.pending "for some reason"
      r `shouldSatisfy` any (== "     # PENDING: for some reason")

  describe "describe" $ do
    let testSpec = do
          H.describe "some subject" $ do
            H.it "foo" True
            H.it "bar" True
            H.it "baz" True
    it "takes a description of what the behavior is for" $ do
      r <- runSpec testSpec
      r `shouldSatisfy` any (== "some subject")

    it "groups behaviors for what's being described" $ do
      r <- filter (isPrefixOf "  - ") `fmap` runSpec testSpec
      length r `shouldBe` 3

    it "can be nested" $ do
      let [SpecGroup foo [SpecGroup bar [SpecItem baz _]]] = runSpecM $ do
            H.describe "foo" $ do
              H.describe "bar" $ do
                H.it "baz" True
      (foo, bar, baz) `shouldBe` ("foo", "bar", "baz")

  describe "it" $ do
    it "takes a description of a desired behavior" $ do
      let [SpecItem d _] = runSpecM (H.it "whatever" True)
      d `shouldBe` "whatever"

    it "takes an example of that behavior" $ do
      let [SpecItem _ e] = runSpecM (H.it "whatever" True)
      e H.defaultParams `shouldReturn` Success

    it "can use a Bool, HUnit Test, QuickCheck property, or `pending` as an example"
      pending
  where
    runSpec :: H.Spec -> IO [String]
    runSpec = capture__ . H.hspecWith defaultConfig
