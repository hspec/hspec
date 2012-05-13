{-# LANGUAGE OverloadedStrings #-}
module RunSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Run

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "findSpecs" $ do
    context "when specs are not nested" $ do
      it "finds a single spec" $ do
        findSpecs "test-data/single-spec" `shouldReturn` [SpecNode "Foo" True []]

      it "finds several specs" $ do
        findSpecs "test-data/several-specs" `shouldReturn` [SpecNode "Bar" True [], SpecNode "Baz" True [], SpecNode "Foo" True []]

    context "when specs are nested" $ do
      it "finds a single spec" $ do
        findSpecs "test-data/single-spec-nested" `shouldReturn` [SpecNode "Foo" False [SpecNode "Bar" True []]]

  where
    context = describe
