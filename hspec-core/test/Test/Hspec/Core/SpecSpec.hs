{-# LANGUAGE CPP #-}
module Test.Hspec.Core.SpecSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Spec (Item(..), Result(..), ResultStatus(..))
import qualified Test.Hspec.Core.Runner as H
import           Test.Hspec.Core.Spec (Tree(..))

import qualified Test.Hspec.Core.Spec as H

extract :: (Item () -> a) -> H.Spec -> IO [Tree () a]
extract f = fmap (H.bimapForest (const ()) f) . fmap snd . H.runSpecM

runSpec :: H.Spec -> IO [String]
runSpec = captureLines . H.hspecResult

spec :: Spec
spec = do
  let
    runSpecM :: H.SpecWith a -> IO [H.SpecTree a]
    runSpecM = fmap snd . H.runSpecM

    runItem :: Item () -> IO Result
    runItem item = itemExample item defaultParams ($ ()) noOpProgressCallback

  describe "getSpecDescriptionPath" $ do
    it "returns the spec path" $ do
      let descriptionPathShouldBe xs =
            H.getSpecDescriptionPath >>= H.runIO . (`shouldBe` xs)
      void . runSpecM $ do
        descriptionPathShouldBe []
        H.describe "foo" $ do
          H.describe "bar" $ do
            descriptionPathShouldBe ["foo", "bar"]
            H.it "baz" True

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
        d `shouldBe` "Test.Hspec.Core.SpecSpec[" ++ show (pred __LINE__ :: Int) ++ ":33]"

  describe "xdescribe" $ do
    it "creates a tree of pending spec items" $ do
      [Node _ [Leaf item]] <- runSpecM (H.xdescribe "" $ H.it "whatever" True)
      runItem item `shouldReturn` Result "" (Pending Nothing Nothing)

  describe "it" $ do
    it "takes a description of a desired behavior" $ do
      [Leaf item] <- runSpecM (H.it "whatever" True)
      itemRequirement item `shouldBe` "whatever"

    it "takes an example of that behavior" $ do
      [Leaf item] <- runSpecM (H.it "whatever" True)
      runItem item `shouldReturn` Result "" Success

    it "adds source locations" $ do
      [Leaf item] <- runSpecM (H.it "foo" True)
      let location = mkLocation __FILE__ (pred __LINE__) 32
      itemLocation item `shouldBe` location

  describe "xit" $ do
    it "creates a pending spec item" $ do
      [Leaf item] <- runSpecM (H.xit "whatever" True)
      runItem item `shouldReturn` Result "" (Pending Nothing Nothing)

  describe "pending" $ do
    it "specifies a pending example" $ do
      r <- runSpec $ do
        H.it "foo" H.pending
      r `shouldSatisfy` any (== "  # PENDING: No reason given")

  describe "pendingWith" $ do
    it "specifies a pending example with a reason for why it's pending" $ do
      r <- runSpec $ do
        H.it "foo" $ do
          H.pendingWith "for some reason"
      r `shouldSatisfy` any (== "  # PENDING: for some reason")

  describe "focus" $ do
    it "focuses spec items" $ do
      items <- extract itemIsFocused $ H.focus $ do
        H.it "is focused and will run" True
        H.it "is also focused and will also run" True
      items `shouldBe` [Leaf True, Leaf True]

    context "when applied to a spec with focused spec items" $ do
      it "has no effect" $ do
        items <- extract itemIsFocused $ H.focus $ do
          H.focus $ H.it "is focused and will run" True
          H.it "is not focused and will not run" True
        items `shouldBe` [Leaf True, Leaf False]

  describe "parallel" $ do
    it "marks examples for parallel execution" $ do
      items <- extract itemIsParallelizable . H.parallel $ H.it "whatever" H.pending
      items `shouldBe` [Leaf $ Just True]

    it "is applied recursively" $ do
      items <- extract itemIsParallelizable . H.parallel $ do
        H.describe "foo" $ do
          H.describe "bar" $ do
            H.it "baz" H.pending
      items `shouldBe` [Node "foo" [Node "bar" [Leaf $ Just True]]]

  describe "sequential" $ do
    it "marks examples for sequential execution" $ do
      items <- extract itemIsParallelizable . H.sequential $ H.it "whatever" H.pending
      items `shouldBe` [Leaf $ Just False]

    it "takes precedence over a later `parallel`" $ do
      items <- extract itemIsParallelizable . H.parallel . H.sequential $ H.it "whatever" H.pending
      items `shouldBe` [Leaf $ Just False]
