{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Hspec.Core.Clock (
  Seconds(..)
, toMicroseconds
, getMonotonicTime
, measure
, sleep
, timeout
) where

import           Text.Printf
import           System.Clock
import           Control.Concurrent
import qualified System.Timeout as System

newtype Seconds = Seconds Double
  deriving (Eq, Show, Num, Fractional, PrintfArg)

toMicroseconds :: Seconds -> Int
toMicroseconds (Seconds s) = floor (s * 1000000)

getMonotonicTime :: IO Seconds
getMonotonicTime = do
  t <- getTime Monotonic
  return $ Seconds ((fromIntegral . toNanoSecs $ t) / 1000000000)

measure :: IO a -> IO (Seconds, a)
measure action = do
  t0 <- getMonotonicTime
  a <- action
  t1 <- getMonotonicTime
  return (t1 - t0, a)

sleep :: Seconds -> IO ()
sleep = threadDelay . toMicroseconds

timeout :: Seconds -> IO a -> IO (Maybe a)
timeout = System.timeout . toMicroseconds
