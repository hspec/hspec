{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Clock (
  Seconds(..)
, toMilliseconds
, toMicroseconds
, getMonotonicTime
, measure
, sleep
, timeout
) where

#if MIN_VERSION_base(4,11,0) && !defined(__MHS__)
#define HAS_GET_MONOTONIC_TIME
#endif

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Text.Printf
import           Control.Concurrent
import qualified System.Timeout as System

#ifdef HAS_GET_MONOTONIC_TIME
import qualified GHC.Clock as GHC
#else
import           Data.Time.Clock.POSIX
#endif

newtype Seconds = Seconds Double
  deriving (Eq, Show, Ord, Num, Fractional, PrintfArg)

toMilliseconds :: Seconds -> Int
toMilliseconds (Seconds s) = floor (s * 1000)

toMicroseconds :: Seconds -> Int
toMicroseconds (Seconds s) = floor (s * 1000000)

getMonotonicTime :: IO Seconds
#ifdef HAS_GET_MONOTONIC_TIME
getMonotonicTime = Seconds <$> GHC.getMonotonicTime
#else
getMonotonicTime = do
  t <- getPOSIXTime
  return $ Seconds (realToFrac t)
#endif

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
