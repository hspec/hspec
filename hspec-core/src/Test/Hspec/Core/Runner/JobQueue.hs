{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.Core.Runner.JobQueue (
  MonadIO
, Job
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

type Job m progress a = (progress -> m ()) -> m a

data Concurrency = Sequential | Concurrent

newtype JobQueue progress a = JobQueue (Chan (QueuedJob progress a))

data QueuedJob progress a = QueuedJob {
  action :: Job IO progress a
, result :: MVar (Result progress a)
}

data Result progress a = Partial progress | Final (Either SomeException a)

withJobQueue :: Int -> (JobQueue progress a -> IO r) -> IO r
withJobQueue concurrency action = do
  chan <- newChan
  let
    worker :: IO Void
    worker = forever $ readChan chan >>= run

    startWorkers :: IO [Async Void]
    startWorkers = replicateM concurrency $ async worker

  bracket startWorkers cancelMany $ \ _ -> do
    action $ JobQueue chan

enqueueJob :: JobQueue progress a -> Concurrency -> Job IO progress a -> IO (Job IO progress (Either SomeException a))
enqueueJob queue = \ case
  Sequential -> runSequential
  Concurrent -> runConcurrent queue

runSequential :: Job IO progress a -> IO (Job IO progress (Either SomeException a))
runSequential action = return $ safeTry . action

runConcurrent :: MonadIO m => JobQueue progress a -> Job IO progress a -> IO (Job m progress (Either SomeException a))
runConcurrent (JobQueue chan) action = do
  result <- newEmptyMVar
  writeChan chan $ QueuedJob action result
  return $ waitForResult result

run :: forall progress a. QueuedJob progress a -> IO ()
run QueuedJob {..} = do
  safeTry (action partialResult) >>= replaceMVar result . Final
  where
    partialResult :: progress -> IO ()
    partialResult = replaceMVar result . Partial

waitForResult :: forall m progress a. MonadIO m => MVar (Result progress a) -> Job m progress (Either SomeException a)
waitForResult result notifyPartial = loop
  where
    loop :: m (Either SomeException a)
    loop = do
      r <- liftIO (takeMVar result)
      case r of
        Partial progress -> notifyPartial progress >> loop
        Final finalResult -> return finalResult

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mvar p = tryTakeMVar mvar >> putMVar mvar p
