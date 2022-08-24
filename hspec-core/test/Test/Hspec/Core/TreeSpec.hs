module Test.Hspec.Core.TreeSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Tree

spec :: Spec
spec = do
  describe "toModuleName" $ do
    it "derives a module name from a FilePath" $ do
      toModuleName "src/Foo/Bar.hs" `shouldBe` "Foo.Bar"
