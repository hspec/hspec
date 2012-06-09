module Test.Hspec.MonadicSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           System.IO.Silently

import qualified Test.Hspec.Monadic as H

main :: IO ()
main = hspecX spec

run :: H.Specs -> IO [String]
run = fmap (lines . fst) . capture . H.hspec

spec :: Specs
spec = do

  describe "A pending example" $ do
    it "is specified with the \"pending\" function" $ do
      r <- run $ do
        H.it "foo" $ do
          H.pending
      r `shouldSatisfy` any (== "     # PENDING: No reason given")

    it "includes an optional message in the report" $ do
      r <- run $ do
        H.it "foo" $ do
          H.pending "for some reason"
      r `shouldSatisfy` any (== "     # PENDING: for some reason")

  describe "A failing example" $ do
    it "is reported" $ do
      r <- run $ do
        H.describe "Foo" $ do
          H.it "does something" False
      r `shouldSatisfy` any (== "1) Foo does something FAILED")

    context "when nested" $ do
      it "is reported" $ do
        r <- run $ do
          H.describe "Foo" $ H.describe "Bar" $ H.describe "baz" $ H.context "when condition" $ do
            H.it "does something" False
        r `shouldSatisfy` any (== "1) Foo - Bar - baz - when condition - does something FAILED")

  where
    context = describe
