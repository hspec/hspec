{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Clock (
  Seconds(..)
, toMilliseconds
, toMicroseconds
, getMonotonicTime
, measure
, measureWithTimeout
, sleep
, timeout
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Text.Printf
import           Control.Concurrent
import qualified System.Timeout as System

#if MIN_VERSION_base(4,11,0)
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
#if MIN_VERSION_base(4,11,0)
getMonotonicTime = Seconds <$> GHC.getMonotonicTime
#else
getMonotonicTime = do
  t <- getPOSIXTime
  return $ Seconds (realToFrac t)
#endif

measure :: IO a -> IO (Seconds, a)
measure = fmap (second fromJust) . measureWithTimeout Nothing

measureWithTimeout :: Maybe Seconds -> IO a -> IO (Seconds, Maybe a)
measureWithTimeout maxTime action = do
  t0 <- getMonotonicTime
  a <- maybe (fmap Just) timeout maxTime action
  t1 <- getMonotonicTime
  return (t1 - t0, a)

sleep :: Seconds -> IO ()
sleep = threadDelay . toMicroseconds

timeout :: Seconds -> IO a -> IO (Maybe a)
timeout = System.timeout . toMicroseconds
