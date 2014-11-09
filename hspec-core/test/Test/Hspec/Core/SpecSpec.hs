module Test.Hspec.Core.SpecSpec (main, spec) where

import           Helper

import           Test.Hspec.Core.Spec (Item(..), Result(..))
import qualified Test.Hspec.Core.Runner as H
import           Test.Hspec.Core.Spec (Tree(..), runSpecM)

import qualified Test.Hspec.Core.Spec as H

main :: IO ()
main = hspec spec

runSpec :: H.Spec -> IO [String]
runSpec = captureLines . H.hspecResult

spec :: Spec
spec = do
  describe "describe" $ do
    it "can be nested" $ do
      [Node foo [Node bar [Leaf _]]] <- runSpecM $ do
        H.describe "foo" $ do
          H.describe "bar" $ do
            H.it "baz" True
      (foo, bar) `shouldBe` ("foo", "bar")

    context "when no description is given" $ do
      it "uses a default description" $ do
        [Node d _] <- runSpecM (H.describe "" (pure ()))
        d `shouldBe` "(no description given)"

  describe "it" $ do
    it "takes a description of a desired behavior" $ do
      [Leaf item] <- runSpecM (H.it "whatever" True)
      itemRequirement item `shouldBe` "whatever"

    it "takes an example of that behavior" $ do
      [Leaf item] <- runSpecM (H.it "whatever" True)
      itemExample item defaultParams ($ ()) noOpProgressCallback `shouldReturn` Success

    context "when no description is given" $ do
      it "uses a default description" $ do
        [Leaf item] <- runSpecM (H.it "" True)
        itemRequirement item `shouldBe` "(unspecified behavior)"

  describe "example" $ do
    it "fixes the type of an expectation" $ do
      r <- runSpec $ do
        H.it "foo" $ H.example $ do
          pure ()
      r `shouldSatisfy` any (== "1 example, 0 failures")

  describe "pending" $ do
    it "specifies a pending example" $ do
      r <- runSpec $ do
        H.it "foo" H.pending
      r `shouldSatisfy` any (== "     # PENDING: No reason given")

  describe "pendingWith" $ do
    it "specifies a pending example with a reason for why it's pending" $ do
      r <- runSpec $ do
        H.it "foo" $ do
          H.pendingWith "for some reason"
      r `shouldSatisfy` any (== "     # PENDING: for some reason")

  describe "parallel" $ do
    it "marks examples for parallel execution" $ do
      [Leaf item] <- runSpecM . H.parallel $ H.it "whatever" True
      itemIsParallelizable item `shouldBe` True

    it "is applied recursively" $ do
      [Node _ [Node _ [Leaf item]]] <- runSpecM . H.parallel $ do
        H.describe "foo" $ do
          H.describe "bar" $ do
            H.it "baz" True
      itemIsParallelizable item `shouldBe` True
