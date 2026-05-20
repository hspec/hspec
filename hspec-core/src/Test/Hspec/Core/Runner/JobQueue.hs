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
import           Data.Void (Void)
import           Control.Concurrent.Async
import           Test.Hspec.Core.Compat
import           Test.Hspec.Core.Util (safeTry)

import           Control.Concurrent

import           Control.Monad.IO.Class

type Job m progress a = (progress -> m ()) -> m a

data Concurrency = Sequential | Concurrent

newtype JobQueue progress a = JobQueue (Chan (QueuedJob progress a))

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

enqueueJob :: forall progress a. JobQueue progress a -> Concurrency -> Job IO progress a -> IO (Job IO progress (Either SomeException a))
enqueueJob (JobQueue chan) concurrency action = do
  result <- newEmptyMVar
  let
    job :: QueuedJob progress a
    job = QueuedJob action result
  case concurrency of
    Sequential -> do
      return $ safeTry . action
    _ -> do
      writeChan chan job
      return $ waitForResult result

data Result progress a = Partial progress | Final (Either SomeException a)

data QueuedJob progress a = QueuedJob {
  action :: Job IO progress a
, result :: MVar (Result progress a)
}

run :: forall progress a. QueuedJob progress a -> IO ()
run QueuedJob {..} = do
  r <- safeTry (action partialResult)
  replaceMVar result $ Final r
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
