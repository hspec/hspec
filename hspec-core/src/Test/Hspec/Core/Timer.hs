module Test.Hspec.Core.Timer (withTimer) where

import           Data.IORef
import           Data.Time.Clock.POSIX
import           Control.Concurrent
import           Control.Exception
import           Control.Monad

import           Control.Concurrent.Async

withTimer :: POSIXTime -> (IO Bool -> IO a) -> IO a
withTimer delay action = do
  ref <- newIORef False
  bracket (async $ worker delay ref) cancel $ \_ -> do
    action $ atomicModifyIORef ref (\a -> (False, a))

sleep :: POSIXTime -> IO ()
sleep = threadDelay . floor . (* 1000000)

worker :: POSIXTime -> IORef Bool -> IO ()
worker delay ref = do
  forever $ do
    sleep delay
    writeIORef ref True
