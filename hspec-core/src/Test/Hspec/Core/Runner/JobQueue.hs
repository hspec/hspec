{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.Core.Runner.JobQueue (
  Job
, Concurrency(..)
, JobQueue
, withJobQueue
, enqueueJob
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Void (Void)

import           Control.Concurrent
import           Control.Concurrent.Async (Async, async, cancelMany)

import           Control.Monad.IO.Class

import           Test.Hspec.Core.Util (safeTry)

type Job progress a = (progress -> IO ()) -> IO a

data Concurrency = Sequential | Concurrent

newtype JobQueue = JobQueue (Chan (IO ()))

data Result progress a = Partial progress | Final (Either SomeException a)

withJobQueue :: Int -> (JobQueue -> IO r) -> IO r
withJobQueue concurrency action = do
  chan <- newChan
  let
    worker :: IO Void
    worker = forever $ join (readChan chan)

    startWorkers :: IO [Async Void]
    startWorkers = replicateM concurrency $ async worker

  bracket startWorkers cancelMany $ \ _ -> do
    action $ JobQueue chan

enqueueJob :: JobQueue -> Concurrency -> Job progress a -> IO (Job progress (Either SomeException a))
enqueueJob queue = \ case
  Sequential -> runSequential
  Concurrent -> runConcurrent queue

runSequential :: Job progress a -> IO (Job progress (Either SomeException a))
runSequential action = return $ safeTry . action

runConcurrent :: JobQueue -> Job progress a -> IO (Job progress (Either SomeException a))
runConcurrent (JobQueue chan) action = do
  result <- newEmptyMVar
  let
    partialResult = replaceMVar result . Partial
  writeChan chan $ do
    safeTry (action partialResult) >>= replaceMVar result . Final
  return $ waitForResult result

waitForResult :: forall progress a. MVar (Result progress a) -> Job progress (Either SomeException a)
waitForResult result notifyPartial = loop
  where
    loop :: IO (Either SomeException a)
    loop = do
      r <- liftIO (takeMVar result)
      case r of
        Partial progress -> notifyPartial progress >> loop
        Final finalResult -> return finalResult

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mvar p = tryTakeMVar mvar >> putMVar mvar p
