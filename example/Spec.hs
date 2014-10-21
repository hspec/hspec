module Main (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.IORef

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "reverse" $ do
    it "reverses a list" $ do
      reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    it "gives the original list, if applied twice" $ property $
      \xs -> (reverse . reverse) xs == (xs :: [Int])

  describe "retry test" $ do
    ref <- runIO $ newIORef (0::Int)
    it "retry" $ do
      let incr :: IO Int
          incr = do
            val <- readIORef ref
            writeIORef ref (val+1)
            return val
      retryWith 11 $ do
        incr `shouldReturn` (10::Int)
