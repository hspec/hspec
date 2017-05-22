{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Hspec.Core.Timer (Seconds, toMicroseconds, sleep, withTimer) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Concurrent.Async

import           Test.Hspec.Core.Compat

newtype Seconds = Seconds Double
  deriving (Num, Fractional)

toMicroseconds :: Seconds -> Int
toMicroseconds (Seconds s) = floor (s * 1000000)

withTimer :: Seconds -> (IO Bool -> IO a) -> IO a
withTimer delay action = do
  ref <- newIORef False
  bracket (async $ worker delay ref) cancel $ \_ -> do
    action $ atomicModifyIORef ref (\a -> (False, a))

sleep :: Seconds -> IO ()
sleep = threadDelay . toMicroseconds

worker :: Seconds -> IORef Bool -> IO ()
worker delay ref = do
  forever $ do
    sleep delay
    atomicWriteIORef ref True
