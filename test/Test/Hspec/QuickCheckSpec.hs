module Test.Hspec.QuickCheckSpec (main, spec) where

import           Test.Hspec.Meta
import           System.IO.Silently

import qualified Test.Hspec as H
import qualified Test.Hspec.Runner as H
import qualified Test.Hspec.QuickCheck as H

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
