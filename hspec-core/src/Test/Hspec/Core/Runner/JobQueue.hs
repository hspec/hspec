{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0)
-- Control.Concurrent.QSem is deprecated in base-4.6.0.*
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Test.Hspec.Core.Runner.JobQueue (
  MonadIO
, Job
, Concurrency(..)
, JobQueue
, withJobQueue
, enqueueJob
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (Monad)
import qualified Test.Hspec.Core.Compat as M

import           Control.Concurrent
import           Control.Concurrent.Async (Async, AsyncCancelled(..), async, waitCatch, asyncThreadId)

import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.IO.Class as M

-- for compatibility with GHC < 7.10.1
type Monad m = (Functor m, Applicative m, M.Monad m)
type MonadIO m = (Monad m, M.MonadIO m)

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

    cancelMany :: [Async a] -> IO ()
    cancelMany jobs = do
      mapM_ notifyCancel jobs
      mapM_ waitCatch jobs

    notifyCancel :: Async a -> IO ()
    notifyCancel = flip throwTo AsyncCancelled . asyncThreadId

newSemaphore :: Int -> IO Semaphore
newSemaphore capacity = do
  sem <- newQSem capacity
  return $ Semaphore (waitQSem sem) (signalQSem sem)

enqueueJob :: MonadIO m => JobQueue -> Concurrency -> Job IO progress a -> IO (Job m progress a)
enqueueJob (JobQueue sem cancelQueue) concurrency = case concurrency of
  Sequential -> runSequentially cancelQueue
  Concurrent -> runConcurrently sem cancelQueue

runSequentially :: forall m progress a. MonadIO m => CancelQueue -> Job IO progress a -> IO (Job m progress a)
runSequentially cancelQueue action = do
  barrier <- newEmptyMVar
  let
    wait :: IO ()
    wait = takeMVar barrier

    signal :: m ()
    signal = liftIO $ putMVar barrier ()

  job <- runConcurrently (Semaphore wait pass) cancelQueue action
  return $ \ notifyPartial -> signal >> job notifyPartial

data Result progress a = Partial progress | Return a

runConcurrently :: forall m progress a. MonadIO m => Semaphore -> CancelQueue -> Job IO progress a -> IO (Job m progress a)
runConcurrently (Semaphore wait signal) cancelQueue action = do
  result :: MVar (Result progress a) <- newEmptyMVar
  let
    worker :: IO ()
    worker = bracket_ wait signal $ do
      action partialResult >>= finalResult
      where
        partialResult :: progress -> IO ()
        partialResult = replaceMVar result . Partial

        finalResult :: a -> IO ()
        finalResult = replaceMVar result . Return

    waitForResult :: (progress -> m ()) -> m a
    waitForResult notifyPartial = do
      r <- liftIO (takeMVar result)
      case r of
        Partial progress -> notifyPartial progress >> waitForResult notifyPartial
        Return a -> return a

    pushOnCancelQueue :: Async () -> IO ()
    pushOnCancelQueue = (modifyIORef cancelQueue . (:))

  _ <- bracket (async $ interruptible worker) pushOnCancelQueue return

  return waitForResult

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mvar p = tryTakeMVar mvar >> putMVar mvar p
