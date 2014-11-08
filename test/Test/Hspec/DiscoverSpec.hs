{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Test.Hspec.DiscoverSpec (main, spec) where

import           Helper
import           Data.String
import           Data.String.Builder

import qualified Test.Hspec.Core.Spec as H
import           Test.Hspec.Core (Tree(..), Item(..), Location(..), LocationAccuracy(..), runSpecM)
import qualified Test.Hspec.Discover as H

infix 1 `shouldHaveLocation`

shouldHaveLocation :: Item a -> (String, Int) -> Expectation
item `shouldHaveLocation` (src, line) = itemLocation item `shouldBe` Just (Location src line 0 BestEffort)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "postProcessSpec" $ do
    it "adds heuristic source locations" $ do
      let c = build $ do
            ""
            strlit "foo"
            ""
            strlit "bar"
            ""
            strlit "baz"
      withFileContent c $ \src -> do
        [Leaf item1, Leaf item2, Leaf item3] <- runSpecM . H.postProcessSpec src $ do
          H.it "foo" True
          H.it "bar" True
          H.it "baz" True
        item1 `shouldHaveLocation` (src, 2)
        item2 `shouldHaveLocation` (src, 4)
        item3 `shouldHaveLocation` (src, 6)

    context "when same requirement is used multiple times" $ do
      it "assigns locations sequentially" $ do
        let c = build $ do
              strlit "foo"
              strlit "foo"
              strlit "foo"
        withFileContent c $ \src -> do
          [Leaf item1, Leaf item2, Leaf item3] <- runSpecM . H.postProcessSpec src $ do
            H.it "foo" True
            H.it "foo" True
            H.it "foo" True
          item1 `shouldHaveLocation` (src, 1)
          item2 `shouldHaveLocation` (src, 2)
          item3 `shouldHaveLocation` (src, 3)

      context "when a requirement occurs more often in the spec tree than in the source file" $ do
        it "assigns Nothing" $ do
          let c = build $ do
                strlit "foo"
                strlit "foo"
          withFileContent c $ \src -> do
            [Leaf item1, Leaf item2, Leaf item3] <- runSpecM . H.postProcessSpec src $ do
              H.it "foo" True
              H.it "foo" True
              H.it "foo" True
            itemLocation item1 `shouldBe` Nothing
            itemLocation item2 `shouldBe` Nothing
            itemLocation item3 `shouldBe` Nothing
  where
    strlit :: String -> Builder
    strlit = fromString . show
