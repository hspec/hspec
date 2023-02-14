{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Stability: provisional
module Test.Hspec.Core.Hooks (
-- * Types
  Spec
, SpecWith
, ActionWith
-- * Hooks
, before
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

import           Control.Concurrent

import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Tree
import           Test.Hspec.Core.Spec.Monad

-- | Run a custom action before every spec item.
before :: IO a -> SpecWith a -> Spec
before action = around (action >>=)

-- | Run a custom action before every spec item.
before_ :: IO () -> SpecWith a -> SpecWith a
before_ action = around_ (action >>)

-- | Run a custom action before every spec item.
beforeWith :: (b -> IO a) -> SpecWith a -> SpecWith b
beforeWith action = aroundWith $ \e x -> action x >>= e

-- | Run a custom action before the first spec item.
beforeAll :: HasCallStack => IO a -> SpecWith a -> Spec
beforeAll action spec = do
  mvar <- runIO (newMVar Empty)
  before (memoize mvar action) spec

-- | Run a custom action before the first spec item.
beforeAll_ :: HasCallStack => IO () -> SpecWith a -> SpecWith a
beforeAll_ action spec = do
  mvar <- runIO (newMVar Empty)
  before_ (memoize mvar action) spec

-- | Run a custom action with an argument before the first spec item.
beforeAllWith :: HasCallStack => (b -> IO a) -> SpecWith a -> SpecWith b
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
after :: ActionWith a -> SpecWith a -> SpecWith a
after action = aroundWith $ \e x -> e x `finally` action x

-- | Run a custom action after every spec item.
after_ :: IO () -> SpecWith a -> SpecWith a
after_ action = after $ \_ -> action

-- | Run a custom action before and/or after every spec item.
around :: (ActionWith a -> IO ()) -> SpecWith a -> Spec
around action = aroundWith $ \e () -> action e

-- | Run a custom action after the last spec item.
afterAll :: HasCallStack => ActionWith a -> SpecWith a -> SpecWith a
afterAll action = aroundAllWith (\ hook a -> hook a >> action a)

-- | Run a custom action after the last spec item.
afterAll_ :: HasCallStack => IO () -> SpecWith a -> SpecWith a
afterAll_ action = mapSpecForest (return . NodeWithCleanup callSite action)

-- | Run a custom action before and/or after every spec item.
around_ :: (IO () -> IO ()) -> SpecWith a -> SpecWith a
around_ action = aroundWith $ \e a -> action (e a)

-- | Run a custom action before and/or after every spec item.
aroundWith :: (ActionWith a -> ActionWith b) -> SpecWith a -> SpecWith b
aroundWith = mapSpecItem_ . modifyHook

modifyHook :: (ActionWith a -> ActionWith b) -> Item a -> Item b
modifyHook action item = item {
    itemExample = \ params hook -> itemExample item params (hook . action)
  }

-- | Wrap an action around the given spec.
aroundAll :: HasCallStack => (ActionWith a -> IO ()) -> SpecWith a -> Spec
aroundAll action = aroundAllWith $ \ e () -> action e

-- | Wrap an action around the given spec.
aroundAll_ :: HasCallStack => (IO () -> IO ()) -> SpecWith a -> SpecWith a
aroundAll_ action spec = do
  (acquire, release) <- runIO $ decompose (action .)
  beforeAll_ (acquire ()) $ afterAll_ release spec

-- | Wrap an action around the given spec. Changes the arg type inside.
aroundAllWith :: forall a b. HasCallStack => (ActionWith a -> ActionWith b) -> SpecWith a -> SpecWith b
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
      acquireHasNotBeenCalled <- isEmptyMVar acquired -- NOTE: This can happen if an outer beforeAll fails
      unless acquireHasNotBeenCalled $ do
        signal doCleanupNow
        r <- takeMVar released
        case r of
          Released -> pass
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
mapSubject :: (b -> a) -> SpecWith a -> SpecWith b
mapSubject f = aroundWith (. f)

-- | Ignore the subject under test for a given spec.
ignoreSubject :: SpecWith () -> SpecWith a
ignoreSubject = mapSubject (const ())
