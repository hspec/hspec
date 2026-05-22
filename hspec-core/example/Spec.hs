module Main (main, spec) where

import Prelude
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Runner
import Test.Hspec.Expectations
import Test.QuickCheck
import Control.Monad
import Control.Concurrent

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  replicateM_ 10 $ do
    it "foo" $ do
      threadDelay 1000000
