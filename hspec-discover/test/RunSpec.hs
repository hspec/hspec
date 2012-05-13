{-# LANGUAGE OverloadedStrings #-}
module RunSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Run

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "findSpecs" $ do
    it "finds a single spec" $ do
      findSpecs "test-data/single-spec" `shouldReturn` [SpecNode "Foo" True []]
