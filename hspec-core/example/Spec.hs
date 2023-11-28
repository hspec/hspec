module Main (main, spec) where

import Prelude
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Runner
import Test.Hspec.Expectations
import Test.QuickCheck hiding (Discard)
import Control.Arrow

import Test.Hspec.Tags (tag)
import qualified Test.Hspec.Tags as Tags

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Tags.use
  Tags.exclude "slow"
  describe ".." $ do
    it "reverses a list" $ do
     reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    it "gives the original list, if applied twice" >>> tag "slow" $ property $
      \ xs -> (reverse . reverse) xs == (xs :: [Int])
