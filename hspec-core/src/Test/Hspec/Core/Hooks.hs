{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Stability: provisional
module Test.Hspec.Core.Hooks (
  before
, before_
, beforeWith
, beforeAll
, beforeAll_
, beforeAllWith
, after
, after_
, afterAll
, afterAll_
, around
, around_
, aroundWith
, aroundAll
, aroundAll_
, aroundAllWith

, mapSubject
, ignoreSubject

#ifdef TEST
, decompose
#endif
) where

import           Prelude ()
import           Test.Hspec.Core.Compat
import           Data.CallStack (HasCallStack)

import           Control.Exception (SomeException, finally, throwIO, try)
import           Control.Concurrent

import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Tree
import           Test.Hspec.Core.Spec.Monad

-- | Run a custom action before every spec item.
before :: Monad m => m a -> SpecWith m a -> SpecWith m ()
before action = around (action >>=)

-- | Run a custom action before every spec item.
before_ :: Monad m => m () -> SpecWith m a -> SpecWith m a
before_ action = around_ (action >>)

-- | Run a custom action before every spec item.
beforeWith :: Monad m => (b -> m a) -> SpecWith m a -> SpecWith m b
beforeWith action = aroundWith $ \e x -> action x >>= e

-- | Run a custom action before the first spec item.
beforeAll :: HasCallStack => IO a -> SpecWith_ a -> Spec
beforeAll action spec = do
  mvar <- runIO (newMVar Empty)
  before (memoize mvar action) spec

-- | Run a custom action before the first spec item.
beforeAll_ :: HasCallStack => IO () -> SpecWith_ a -> SpecWith_ a
beforeAll_ action spec = do
  mvar <- runIO (newMVar Empty)
  before_ (memoize mvar action) spec

-- | Run a custom action with an argument before the first spec item.
beforeAllWith :: HasCallStack => (b -> IO a) -> SpecWith_ a -> SpecWith_ b
beforeAllWith action spec = do
  mvar <- runIO (newMVar Empty)
  beforeWith (memoize mvar . action) spec

data Memoized a =
    Empty
  | Memoized a
  | Failed SomeException

memoize :: HasCallStack => MVar (Memoized a) -> IO a -> IO a
memoize mvar action = do
  result <- modifyMVar mvar $ \ma -> case ma of
    Empty -> do
      a <- try action
      return (either Failed Memoized a, a)
    Memoized a -> return (ma, Right a)
    Failed _ -> throwIO (Pending Nothing (Just $ "exception in " <> maybe "beforeAll" fst callSite <> "-hook (see previous failure)"))
  either throwIO return result

-- | Run a custom action after every spec item.
after :: ActionWith_ a -> SpecWith IO a -> SpecWith IO a
after action = aroundWith $ \e x -> e x `finally` action x

-- | Run a custom action after every spec item.
after_ :: IO () -> SpecWith IO a -> SpecWith IO a
after_ action = after $ \_ -> action

-- | Run a custom action before and/or after every spec item.
around :: (ActionWith m a -> m ()) -> SpecWith m a -> SpecWith m ()
around action = aroundWith $ \e () -> action e

-- | Run a custom action after the last spec item.
afterAll :: HasCallStack => ActionWith_ a -> SpecWith_ a -> SpecWith_ a
afterAll action = aroundAllWith (\ hook a -> hook a >> action a)

-- | Run a custom action after the last spec item.
afterAll_ :: HasCallStack => IO () -> SpecWith_ a -> SpecWith_ a
afterAll_ action = mapSpecForest (return . NodeWithCleanup callSite action)

-- | Run a custom action before and/or after every spec item.
around_ :: (m () -> m ()) -> SpecWith m a -> SpecWith m a
around_ action = aroundWith $ \e a -> action (e a)

-- | Run a custom action before and/or after every spec item.
aroundWith :: (ActionWith m a -> ActionWith m b) -> SpecWith m a -> SpecWith m b
aroundWith action = mapSpecItem action (modifyAroundAction action)

modifyAroundAction :: (ActionWith m a -> ActionWith m b) -> Item m a -> Item m b
modifyAroundAction action item@Item{itemExample = e} =
  item{ itemExample = \params aroundAction -> e params (aroundAction . action) }

-- | Wrap an action around the given spec.
aroundAll :: HasCallStack => (ActionWith_ a -> IO ()) -> SpecWith_ a -> Spec
aroundAll action = aroundAllWith $ \ e () -> action e

-- | Wrap an action around the given spec.
aroundAll_ :: HasCallStack => (IO () -> IO ()) -> SpecWith_ a -> SpecWith_ a
aroundAll_ action spec = do
  (acquire, release) <- runIO $ decompose (action .)
  beforeAll_ (acquire ()) $ afterAll_ release spec

-- | Wrap an action around the given spec. Changes the arg type inside.
aroundAllWith :: forall a b. HasCallStack => (ActionWith_ a -> ActionWith_ b) -> SpecWith_ a -> SpecWith_ b
aroundAllWith action spec = do
  (acquire, release) <- runIO $ decompose action
  beforeAllWith acquire $ afterAll_ release spec

data Acquired a = Acquired a | ExceptionDuringAcquire SomeException
data Released   = Released   | ExceptionDuringRelease SomeException

decompose :: forall a b. ((a -> IO ()) -> b -> IO ()) -> IO (b -> IO a, IO ())
decompose action = do
  doCleanupNow <- newEmptyMVar
  acquired <- newEmptyMVar
  released <- newEmptyMVar

  let
    notify :: Either SomeException () -> IO ()
    -- `notify` is guaranteed to run without being interrupted by an async
    -- exception for the following reasons:
    --
    -- 1. `forkFinally` runs the final action within `mask`
    -- 2. `tryPutMVar` is guaranteed not to be interruptible
    -- 3. `putMVar` is guaranteed not to be interruptible on an empty `MVar`
    notify r = case r of
      Left err -> do
        exceptionDuringAcquire <- tryPutMVar acquired (ExceptionDuringAcquire err)
        putMVar released $ if exceptionDuringAcquire then Released else ExceptionDuringRelease err
      Right () -> do
        putMVar released Released

    forkWorker :: b -> IO ()
    forkWorker b = void . flip forkFinally notify $ do
      flip action b $ \ a -> do
        putMVar acquired (Acquired a)
        waitFor doCleanupNow

    acquire :: b -> IO a
    acquire b = do
      forkWorker b
      r <- readMVar acquired -- This does not work reliably with base < 4.7
      case r of
        Acquired a -> return a
        ExceptionDuringAcquire err -> throwIO err

    release :: IO ()
    release = do
      signal doCleanupNow
      r <- takeMVar released
      case r of
        Released -> return ()
        ExceptionDuringRelease err -> throwIO err

  return (acquire, release)

type BinarySemaphore = MVar ()

signal :: BinarySemaphore -> IO ()
signal = flip putMVar ()

waitFor :: BinarySemaphore -> IO ()
waitFor = takeMVar

-- | Modify the subject under test.
--
-- Note that this resembles a contravariant functor on the first type parameter
-- of `SpecM`.  This is because the subject is passed inwards, as an argument
-- to the spec item.
mapSubject :: (b -> a) -> SpecWith m a -> SpecWith m b
mapSubject f = aroundWith (. f)

-- | Ignore the subject under test for a given spec.
ignoreSubject :: SpecWith m () -> SpecWith m a
ignoreSubject = mapSubject (const ())
