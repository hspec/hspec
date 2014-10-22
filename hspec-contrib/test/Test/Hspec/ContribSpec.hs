module Test.Hspec.ContribSpec (main, spec) where

import           Data.IORef

import           Test.Hspec
import           Test.Hspec.Contrib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "retry test" $ do
    ref <- runIO $ newIORef (0::Int)
    it "retry 11 times, then check the value" $ do
      let incr :: IO Int
          incr = do
            val <- readIORef ref
            writeIORef ref (val+1)
            return val
      retryWith 11 $ do
        incr `shouldReturn` (10::Int)
