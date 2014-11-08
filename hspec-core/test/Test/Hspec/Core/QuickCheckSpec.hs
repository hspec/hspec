module Test.Hspec.Core.QuickCheckSpec (main, spec) where

import           Helper

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Core.Runner as H
import qualified Test.Hspec.Core.QuickCheck as H

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "prop" $ do
    it "is a shortcut to use properties as examples" $ do
      silence . H.hspecResult $ do
        H.describe "read" $ do
          H.prop "is inverse to show" $ \x -> (read . show) x == (x :: Int)
      `shouldReturn` H.Summary 1 0
