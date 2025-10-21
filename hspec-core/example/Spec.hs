module Main (main, spec) where

import Prelude
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Runner
import Test.Hspec.Expectations
import Test.QuickCheck hiding (Discard)
import Control.Arrow

import qualified Test.Hspec.CI as CI

-- import Test.Hspec.Tags (tag)
-- import qualified Test.Hspec.Tags as Tags

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  CI.use
  describe "Prelude.reverse" $ do
    it "reverses a list" $ do
     reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    it "gives the original list, if applied twice" >>> CI.only $ property $
      \ xs -> (reverse . reverse) xs == (xs :: [Int])
