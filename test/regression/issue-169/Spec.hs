module Main (main, spec) where
import           Test.Hspec

import           Control.Monad
import           Data.Word

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hspec" $ do
    it "does not hold a reference to example action" $ do
      let n = (maxBound `div` 1024) :: Word32
      forM_ [1 .. n] $ \x ->
        when (x == 0) $ expectationFailure "failed"
