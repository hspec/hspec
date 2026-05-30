{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

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

import           Control.Concurrent
import           Control.Concurrent.Async (Async, async, waitCatch, cancelMany)

import           Control.Monad.IO.Class

type Job m progress a = (progress -> m ()) -> m a

data Concurrency = Sequential | Concurrent

data JobQueue = JobQueue {
  _semaphore :: Semaphore
, _cancelQueue :: CancelQueue
}

data Semaphore = Semaphore {
  _wait :: IO ()
, _signal :: IO ()
}

type CancelQueue = IORef [Async ()]

withJobQueue :: Int -> (JobQueue -> IO a) -> IO a
withJobQueue concurrency = bracket new cancelAll
  where
    new :: IO JobQueue
    new = JobQueue <$> newSemaphore concurrency <*> newIORef []

    cancelAll :: JobQueue -> IO ()
    cancelAll (JobQueue _ cancelQueue) = readIORef cancelQueue >>= cancelMany

newSemaphore :: Int -> IO Semaphore
newSemaphore capacity = do
  sem <- newQSem capacity
  return $ Semaphore (waitQSem sem) (signalQSem sem)

enqueueJob :: MonadIO m => JobQueue -> Concurrency -> Job IO progress a -> IO (Job m progress (Either SomeException a))
enqueueJob (JobQueue sem cancelQueue) concurrency = case concurrency of
  Sequential -> runSequentially cancelQueue
  Concurrent -> runConcurrently sem cancelQueue

runSequentially :: forall m progress a. MonadIO m => CancelQueue -> Job IO progress a -> IO (Job m progress (Either SomeException a))
runSequentially cancelQueue action = do
  barrier <- newEmptyMVar
  let
    wait :: IO ()
    wait = takeMVar barrier

    signal :: m ()
    signal = liftIO $ putMVar barrier ()

  job <- runConcurrently (Semaphore wait pass) cancelQueue action
  return $ \ notifyPartial -> signal >> job notifyPartial

data Result progress a = Partial progress | Done

runConcurrently :: forall m progress a. MonadIO m => Semaphore -> CancelQueue -> Job IO progress a -> IO (Job m progress (Either SomeException a))
runConcurrently (Semaphore wait signal) cancelQueue action = do
  result :: MVar (Result progress a) <- newEmptyMVar
  let
    worker :: IO a
    worker = bracket_ wait signal $ do
      interruptible (action partialResult) `finally` done
      where
        partialResult :: progress -> IO ()
        partialResult = replaceMVar result . Partial

        done :: IO ()
        done = replaceMVar result Done

    pushOnCancelQueue :: Async a -> IO ()
    pushOnCancelQueue = (modifyIORef cancelQueue . (:) . void)

  job <- bracket (async worker) pushOnCancelQueue return

  return $ waitForResult job result

waitForResult :: forall m progress a. MonadIO m => Async a -> MVar (Result progress a) -> Job m progress (Either SomeException a)
waitForResult job result notifyPartial = loop
  where
    loop :: m (Either SomeException a)
    loop = do
      r <- liftIO (takeMVar result)
      case r of
        Partial progress -> notifyPartial progress >> loop
        Done -> liftIO $ waitCatch job

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mvar p = tryTakeMVar mvar >> putMVar mvar p
