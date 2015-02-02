module Test.Hspec.Contrib.RetrySpec (main, spec) where

import           Data.IORef

import           Test.Hspec
import           Test.Hspec.Contrib.Retry

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
  describe "rollback test" $ do
    ref <- runIO $ newIORef (0::Int)
    total <- runIO $ newIORef (0::Int)
    let incr :: IO Int
        incr = do
          val <- readIORef ref
          writeIORef ref (val+2)
          modifyIORef total (+1)
          return val
        decr :: IO ()
        decr = do
          val <- readIORef ref
          writeIORef ref (val-1)
          return ()
    it "retry with rollback" $ do
      retryWith 4 $
        rollbackWith decr $ -- "decr" is example for rollback.
          incr `shouldReturn` (3::Int)
    it "check total counts of retry" $ do
      readIORef total `shouldReturn` 4
