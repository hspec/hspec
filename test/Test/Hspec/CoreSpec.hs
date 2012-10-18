module Test.Hspec.CoreSpec (main, spec) where

import           Test.Hspec.Meta
import           Util (capture_)
import           Data.List (isPrefixOf)

import qualified Test.Hspec.Internal as H
import qualified Test.Hspec.Core as H
import           Test.Hspec.Runner (defaultConfig)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hspec" $ do
    it "runs a forrest of `SpecTree`s" $ do
      H.hspec [H.it "foo" H.pending] `shouldReturn` ()

  describe "pending" $ do
    it "specifies a pending example" $ do
      r <- runSpec [H.it "foo" H.pending]
      r `shouldSatisfy` any (== "     # PENDING: No reason given")

    it "accepts an optional message, which is included in the report" $ do
      r <- runSpec [H.it "foo" $ H.pending "for some reason"]
      r `shouldSatisfy` any (== "     # PENDING: for some reason")

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
      r <- filter (isPrefixOf "  - ") `fmap` runSpec testSpec
      length r `shouldBe` 3

    describe "a nested description" $ do
        it "has it's own specs"
            (True)

  describe "the \"it\" function" $ do
    it "takes a description of a desired behavior" $
      case H.it "whatever" H.Success of
        H.SpecExample requirement _ -> requirement == "whatever"
        _ -> False

    it "takes an example of that behavior" $ do
      case H.it "whatever" H.Success of
        H.SpecExample _ example -> do
          r <- example H.defaultParams
          r `shouldBe` H.Success
        H.SpecGroup _ _ -> error "unexpected SpecGroup"

    it "can use a Bool, HUnit Test, QuickCheck property, or `pending` as an example"
      pending
  where
    runSpec :: [H.SpecTree] -> IO [String]
    runSpec s = capture_ (H.hspecWith defaultConfig s)
