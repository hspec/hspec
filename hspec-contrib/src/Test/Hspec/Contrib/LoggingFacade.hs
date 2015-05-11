module Test.Hspec.Contrib.LoggingFacade (
  captureLogs
) where

import           Control.Exception
import           Data.IORef
import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink

-- | Capture all logs produced by an IO action.
-- Logs are kept in memory.
captureLogs :: IO a -> IO ([LogRecord], a)
captureLogs action = bracket enter exit act
  where
    logToRef ref record = atomicModifyIORef' ref $ \logs -> (record : logs, ())
    enter = do
      oldSink <- getLogSink
      ref <- newIORef []
      setLogSink $ logToRef ref
      return (oldSink, ref)
    exit (oldSink, _) = setLogSink oldSink
    act (_, ref)  = do
      val <- action
      logs <- readIORef ref
      return (reverse logs, val)
{-# DEPRECATED captureLogs "Use \"Test.Mockery.Logging.captureLogMessages\" from package @mockery@ instead" #-}
