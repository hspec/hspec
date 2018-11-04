{-# LANGUAGE CPP #-}
module Test.Hspec.Core.SpecSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Spec (Item(..), Result(..), ResultStatus(..))
import qualified Test.Hspec.Core.Runner as H
import           Test.Hspec.Core.Spec (Tree(..), runSpecM)

import qualified Test.Hspec.Core.Spec as H

ignoreCleanup :: Tree c a -> Tree () a
ignoreCleanup = H.bimapTree (const ()) id

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
#if MIN_VERSION_base(4,8,1)
        d `shouldBe` "Test.Hspec.Core.SpecSpec[" ++ show (__LINE__ - 2 :: Int) ++ ":33]"
#else
        d `shouldBe` "(no description given)"
#endif

  describe "xdescribe" $ do
    it "creates a tree of pending spec items" $ do
      [Node _ [Leaf item]] <- runSpecM (H.xdescribe "" $ H.it "whatever" True)
      r <- itemExample item defaultParams ($ ()) noOpProgressCallback
      r `shouldBe` Result "" (Pending Nothing Nothing)

  describe "it" $ do
    it "takes a description of a desired behavior" $ do
      [Leaf item] <- runSpecM (H.it "whatever" True)
      itemRequirement item `shouldBe` "whatever"

    it "takes an example of that behavior" $ do
      [Leaf item] <- runSpecM (H.it "whatever" True)
      r <- itemExample item defaultParams ($ ()) noOpProgressCallback
      r `shouldBe` Result "" Success

    it "adds source locations" $ do
      [Leaf item] <- runSpecM (H.it "foo" True)
      let location =
#if MIN_VERSION_base(4,8,1)
            Just $ H.Location __FILE__ (__LINE__ - 3) 32
#else
            Nothing
#endif
      itemLocation item `shouldBe` location

    context "when no description is given" $ do
      it "uses a default description" $ do
        [Leaf item] <- runSpecM (H.it "" True)
#if MIN_VERSION_base(4,8,1)
        itemRequirement item `shouldBe` "Test.Hspec.Core.SpecSpec[" ++ show (__LINE__ - 2 :: Int) ++ ":34]"
#else
        itemRequirement item `shouldBe` "(unspecified behavior)"
#endif

  describe "xit" $ do
    it "creates a pending spec item" $ do
      [Leaf item] <- runSpecM (H.xit "whatever" True)
      r <- itemExample item defaultParams ($ ()) noOpProgressCallback
      r `shouldBe` Result "" (Pending Nothing Nothing)

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
      items <- runSpecM $ H.focus $ do
        H.it "is focused and will run" True
        H.it "is also focused and will also run" True
      map (ignoreCleanup . fmap itemIsFocused) items
        `shouldBe` [Leaf True, Leaf True]

    context "when applied to a spec with focused spec items" $ do
      it "has no effect" $ do
        items <- runSpecM $ H.focus $ do
          H.focus $ H.it "is focused and will run" True
          H.it "is not focused and will not run" True
        map (ignoreCleanup . fmap itemIsFocused) items
          `shouldBe` [Leaf True, Leaf False]

  describe "parallel" $ do
    it "marks examples for parallel execution" $ do
      [Leaf item] <- runSpecM . H.parallel $ H.it "whatever" H.pending
      itemIsParallelizable item `shouldBe` Just True

    it "is applied recursively" $ do
      [Node _ [Node _ [Leaf item]]] <- runSpecM . H.parallel $ do
        H.describe "foo" $ do
          H.describe "bar" $ do
            H.it "baz" H.pending
      itemIsParallelizable item `shouldBe` Just True

  describe "sequential" $ do
    it "marks examples for sequential execution" $ do
      [Leaf item] <- runSpecM . H.sequential $ H.it "whatever" H.pending
      itemIsParallelizable item `shouldBe` Just False

    it "takes precedence over a later `parallel`" $ do
      [Leaf item] <- runSpecM . H.parallel . H.sequential $ H.it "whatever" H.pending
      itemIsParallelizable item `shouldBe` Just False
