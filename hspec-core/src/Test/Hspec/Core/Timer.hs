module Test.Hspec.Core.Timer (withTimer) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Exception
import           Control.Concurrent.Async

import           Test.Hspec.Core.Clock

withTimer :: Seconds -> (IO Bool -> IO a) -> IO a
withTimer delay action = do
  ref <- newIORef False
  bracket (async $ worker delay ref) cancel $ \_ -> do
    action $ atomicModifyIORef ref (\a -> (False, a))

worker :: Seconds -> IORef Bool -> IO ()
worker delay ref = do
  forever $ do
    sleep delay
    atomicWriteIORef ref True
