{-# LANGUAGE CPP, MagicHash, UnboxedTuples, RankNTypes,
    ExistentialQuantification #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DeriveDataTypeable #-}
#endif
{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Async
-- Copyright   :  (c) Simon Marlow 2012
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Simon Marlow <marlowsd@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- This module provides a set of operations for running IO operations
-- asynchronously and waiting for their results.  It is a thin layer
-- over the basic concurrency operations provided by
-- "Control.Concurrent".  The main additional functionality it
-- provides is the ability to wait for the return value of a thread,
-- but the interface also provides some additional safety and
-- robustness over using 'forkIO' threads and @MVar@ directly.
--
-- == High-level API
--
-- @async@'s high-level API spawns /lexically scoped/ threads,
-- ensuring the following key poperties that make it safer to use
-- than using plain 'forkIO':
--
-- 1. No exception is swallowed (waiting for results propagates exceptions).
-- 2. No thread is leaked (left running unintentionally).
--
-- (This is done using the 'Control.Exception.bracket' pattern to work in presence
-- of synchornous and asynchronous exceptions.)
--
-- __Most practical/production code should only use the high-level API__.
--
-- The basic type is @'Async' a@, which represents an asynchronous
-- @IO@ action that will return a value of type @a@, or die with an
-- exception.  An 'Async' is a wrapper around a low-level 'forkIO' thread.
--
-- The fundamental function to spawn threads with the high-level API is
-- 'withAsync'.
--
-- For example, to fetch two web pages at the same time, we could do
-- this (assuming a suitable @getURL@ function):
--
-- > withAsync (getURL url1) $ \a1 -> do
-- >   withAsync (getURL url2) $ \a2 -> do
-- >     page1 <- wait a1
-- >     page2 <- wait a2
-- >     ...
--
-- where 'withAsync' starts the operation in a separate thread, and
-- 'wait' waits for and returns the result.
--
-- * If the operation throws an exception, then that exception is re-thrown
--   by 'wait'. This ensures property (1): No exception is swallowed.
-- * If an exception bubbles up through a 'withAsync', then the 'Async'
--   it spawned is 'cancel'ed. This ensures property (2): No thread is leaked.
--
-- Often we do not care to work manually with 'Async' handles like
-- @a1@ and @a2@. Instead, we want to express high-level objectives like
-- performing two or more tasks concurrently, and waiting for one or all
-- of them to finish.
--
-- For example, the pattern of performing two IO actions concurrently and
-- waiting for both their results is packaged up in a combinator 'concurrently',
-- so we can further shorten the above example to:
--
-- > (page1, page2) <- concurrently (getURL url1) (getURL url2)
-- > ...
--
-- The section __/High-level utilities/__ covers the most
-- common high-level objectives, including:
--
-- * Waiting for 2 results ('concurrently').
-- * Waiting for many results ('mapConcurrently' / 'forConcurrently').
-- * Waiting for the first of 2 results ('race').
-- * Waiting for arbitrary nestings of "all of /N/" and "the first of /N/"
--   results with the 'Concurrently' newtype and its 'Applicative' and
--   'Alternative' instances.
--
-- Click here to scroll to that section:
-- "Control.Concurrent.Async#high-level-utilities".
--
-- == Low-level API
--
-- Some use cases require parallelism that is not lexically scoped.
--
-- For those, the low-level function 'async' can be used as a direct
-- equivalent of 'forkIO':
--
-- > -- Do NOT use this code in production, it has a flaw (explained below).
-- > do
-- >   a1 <- async (getURL url1)
-- >   a2 <- async (getURL url2)
-- >   page1 <- wait a1
-- >   page2 <- wait a2
-- >   ...
--
-- In contrast to 'withAsync', this code has a problem.
--
-- It still fulfills property (1) in that an exception arising from
-- @getUrl@ will be re-thrown by 'wait', but it does not fulfill
-- property (2).
-- Consider the case when the first 'wait' throws an exception; then the
-- second 'wait' will not happen, and the second 'async' may be left
-- running in the background, possibly indefinitely.
--
-- 'withAsync' is like 'async', except that the 'Async' is
-- automatically killed (using 'uninterruptibleCancel') if the
-- enclosing IO operation returns before it has completed.
-- Furthermore, 'withAsync' allows a tree of threads to be built, such
-- that children are automatically killed if their parents die for any
-- reason.
--
-- If you need to use the low-level API, ensure that you gurantee
-- property (2) by other means, such as 'link'ing asyncs that need
-- to die together, and protecting against asynchronous exceptions
-- using 'Control.Exception.bracket', 'Control.Exception.mask',
-- or other functions from "Control.Exception".
--
-- == Miscellaneous
--
-- The 'Functor' instance can be used to change the result of an
-- 'Async'.  For example:
--
-- > ghci> withAsync (return 3) (\a -> wait (fmap (+1) a))
-- > 4
--
-- === Resource exhaustion
--
-- As with all concurrent programming, keep in mind that while
-- Haskell's cooperative ("green") multithreading carries low overhead,
-- spawning too many of them at the same time may lead to resource exhaustion
-- (of memory, file descriptors, or other limited resources), given that the
-- actions running in the threads consume these resources.

-----------------------------------------------------------------------------

module Control.Concurrent.Async (

    -- * Asynchronous actions
    Async,

    -- * High-level API

    -- ** Spawning with automatic 'cancel'ation
    withAsync, withAsyncBound, withAsyncOn, withAsyncWithUnmask,
    withAsyncOnWithUnmask,

    -- ** Querying 'Async's
    wait, poll, waitCatch, asyncThreadId,
    cancel, uninterruptibleCancel, cancelWith, AsyncCancelled(..),

    -- ** #high-level-utilities# High-level utilities
    race, race_,
    concurrently, concurrently_,
    mapConcurrently, forConcurrently,
    mapConcurrently_, forConcurrently_,
    replicateConcurrently, replicateConcurrently_,
    Concurrently(..),
    compareAsyncs,

    -- ** Specialised operations

    -- *** STM operations
    waitSTM, pollSTM, waitCatchSTM,

    -- *** Waiting for multiple 'Async's
    waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel,
    waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel,
    waitEither_,
    waitBoth,

    -- *** Waiting for multiple 'Async's in STM
    waitAnySTM, waitAnyCatchSTM,
    waitEitherSTM, waitEitherCatchSTM,
    waitEitherSTM_,
    waitBothSTM,

    -- * Low-level API

    -- ** Spawning (low-level API)
    async, asyncBound, asyncOn, asyncWithUnmask, asyncOnWithUnmask,

    -- ** Linking
    link, linkOnly, link2, link2Only, ExceptionInLinkedThread(..),

  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent
import qualified Data.Foldable as F
#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif
import Control.Monad
import Control.Applicative
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid(mempty,mappend))
import Data.Traversable
#endif
#if __GLASGOW_HASKELL__ < 710
import Data.Typeable
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Hashable (Hashable(hashWithSalt))

import Data.IORef

import GHC.Exts
import GHC.IO hiding (finally, onException)
import GHC.Conc

-- -----------------------------------------------------------------------------
-- STM Async API


-- | An asynchronous action spawned by 'async' or 'withAsync'.
-- Asynchronous actions are executed in a separate thread, and
-- operations are provided for waiting for asynchronous actions to
-- complete and obtaining their results (see e.g. 'wait').
--
data Async a = Async
  { asyncThreadId :: {-# UNPACK #-} !ThreadId
                  -- ^ Returns the 'ThreadId' of the thread running
                  -- the given 'Async'.
  , _asyncWait    :: STM (Either SomeException a)
  }

instance Eq (Async a) where
  Async a _ == Async b _  =  a == b

instance Ord (Async a) where
  Async a _ `compare` Async b _  =  a `compare` b

instance Hashable (Async a) where
  hashWithSalt salt (Async a _) = hashWithSalt salt a

instance Functor Async where
  fmap f (Async a w) = Async a (fmap (fmap f) w)

-- | Compare two Asyncs that may have different types by their 'ThreadId'.
compareAsyncs :: Async a -> Async b -> Ordering
compareAsyncs (Async t1 _) (Async t2 _) = compare t1 t2

-- | Spawn an asynchronous action in a separate thread.
--
-- Like for 'forkIO', the action may be left running unintentinally
-- (see module-level documentation for details).
--
-- __Use 'withAsync' style functions wherever you can instead!__
async :: IO a -> IO (Async a)
async = inline asyncUsing rawForkIO

-- | Like 'async' but using 'forkOS' internally.
asyncBound :: IO a -> IO (Async a)
asyncBound = asyncUsing forkOS

-- | Like 'async' but using 'forkOn' internally.
asyncOn :: Int -> IO a -> IO (Async a)
asyncOn = asyncUsing . rawForkOn

-- | Like 'async' but using 'forkIOWithUnmask' internally.  The child
-- thread is passed a function that can be used to unmask asynchronous
-- exceptions.
asyncWithUnmask :: ((forall b . IO b -> IO b) -> IO a) -> IO (Async a)
asyncWithUnmask actionWith = asyncUsing rawForkIO (actionWith unsafeUnmask)

-- | Like 'asyncOn' but using 'forkOnWithUnmask' internally.  The
-- child thread is passed a function that can be used to unmask
-- asynchronous exceptions.
asyncOnWithUnmask :: Int -> ((forall b . IO b -> IO b) -> IO a) -> IO (Async a)
asyncOnWithUnmask cpu actionWith =
  asyncUsing (rawForkOn cpu) (actionWith unsafeUnmask)

asyncUsing :: (IO () -> IO ThreadId)
           -> IO a -> IO (Async a)
asyncUsing doFork = \action -> do
   var <- newEmptyTMVarIO
   -- t <- forkFinally action (\r -> atomically $ putTMVar var r)
   -- slightly faster:
   t <- mask $ \restore ->
          doFork $ try (restore action) >>= atomically . putTMVar var
   return (Async t (readTMVar var))

-- | Spawn an asynchronous action in a separate thread, and pass its
-- @Async@ handle to the supplied function.  When the function returns
-- or throws an exception, 'uninterruptibleCancel' is called on the @Async@.
--
-- > withAsync action inner = mask $ \restore -> do
-- >   a <- async (restore action)
-- >   restore (inner a) `finally` uninterruptibleCancel a
--
-- This is a useful variant of 'async' that ensures an @Async@ is
-- never left running unintentionally.
--
-- Note: a reference to the child thread is kept alive until the call
-- to `withAsync` returns, so nesting many `withAsync` calls requires
-- linear memory.
--
withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync = inline withAsyncUsing rawForkIO

-- | Like 'withAsync' but uses 'forkOS' internally.
withAsyncBound :: IO a -> (Async a -> IO b) -> IO b
withAsyncBound = withAsyncUsing forkOS

-- | Like 'withAsync' but uses 'forkOn' internally.
withAsyncOn :: Int -> IO a -> (Async a -> IO b) -> IO b
withAsyncOn = withAsyncUsing . rawForkOn

-- | Like 'withAsync' but uses 'forkIOWithUnmask' internally.  The
-- child thread is passed a function that can be used to unmask
-- asynchronous exceptions.
withAsyncWithUnmask
  :: ((forall c. IO c -> IO c) -> IO a) -> (Async a -> IO b) -> IO b
withAsyncWithUnmask actionWith =
  withAsyncUsing rawForkIO (actionWith unsafeUnmask)

-- | Like 'withAsyncOn' but uses 'forkOnWithUnmask' internally.  The
-- child thread is passed a function that can be used to unmask
-- asynchronous exceptions
withAsyncOnWithUnmask
  :: Int -> ((forall c. IO c -> IO c) -> IO a) -> (Async a -> IO b) -> IO b
withAsyncOnWithUnmask cpu actionWith =
  withAsyncUsing (rawForkOn cpu) (actionWith unsafeUnmask)

withAsyncUsing :: (IO () -> IO ThreadId)
               -> IO a -> (Async a -> IO b) -> IO b
-- The bracket version works, but is slow.  We can do better by
-- hand-coding it:
withAsyncUsing doFork = \action inner -> do
  var <- newEmptyTMVarIO
  mask $ \restore -> do
    t <- doFork $ try (restore action) >>= atomically . putTMVar var
    let a = Async t (readTMVar var)
    r <- restore (inner a) `catchAll` \e -> do
      uninterruptibleCancel a
      throwIO e
    uninterruptibleCancel a
    return r

-- | Wait for an asynchronous action to complete, and return its
-- value.  If the asynchronous action threw an exception, then the
-- exception is re-thrown by 'wait'.
--
-- > wait = atomically . waitSTM
--
{-# INLINE wait #-}
wait :: Async a -> IO a
wait = tryAgain . atomically . waitSTM
  where
    -- See: https://github.com/simonmar/async/issues/14
    tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f

-- | Wait for an asynchronous action to complete, and return either
-- @Left e@ if the action raised an exception @e@, or @Right a@ if it
-- returned a value @a@.
--
-- > waitCatch = atomically . waitCatchSTM
--
{-# INLINE waitCatch #-}
waitCatch :: Async a -> IO (Either SomeException a)
waitCatch = tryAgain . atomically . waitCatchSTM
  where
    -- See: https://github.com/simonmar/async/issues/14
    tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f

-- | Check whether an 'Async' has completed yet.  If it has not
-- completed yet, then the result is @Nothing@, otherwise the result
-- is @Just e@ where @e@ is @Left x@ if the @Async@ raised an
-- exception @x@, or @Right a@ if it returned a value @a@.
--
-- > poll = atomically . pollSTM
--
{-# INLINE poll #-}
poll :: Async a -> IO (Maybe (Either SomeException a))
poll = atomically . pollSTM

-- | A version of 'wait' that can be used inside an STM transaction.
--
waitSTM :: Async a -> STM a
waitSTM a = do
   r <- waitCatchSTM a
   either throwSTM return r

-- | A version of 'waitCatch' that can be used inside an STM transaction.
--
{-# INLINE waitCatchSTM #-}
waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ w) = w

-- | A version of 'poll' that can be used inside an STM transaction.
--
{-# INLINE pollSTM #-}
pollSTM :: Async a -> STM (Maybe (Either SomeException a))
pollSTM (Async _ w) = (Just <$> w) `orElse` return Nothing

-- | Cancel an asynchronous action by throwing the @AsyncCancelled@
-- exception to it, and waiting for the `Async` thread to quit.
-- Has no effect if the 'Async' has already completed.
--
-- > cancel a = throwTo (asyncThreadId a) AsyncCancelled <* waitCatch a
--
-- Note that 'cancel' will not terminate until the thread the 'Async'
-- refers to has terminated. This means that 'cancel' will block for
-- as long said thread blocks when receiving an asynchronous exception.
--
-- For example, it could block if:
--
-- * It's executing a foreign call, and thus cannot receive the asynchronous
-- exception;
-- * It's executing some cleanup handler after having received the exception,
-- and the handler is blocking.
{-# INLINE cancel #-}
cancel :: Async a -> IO ()
cancel a@(Async t _) = throwTo t AsyncCancelled <* waitCatch a

-- | The exception thrown by `cancel` to terminate a thread.
data AsyncCancelled = AsyncCancelled
  deriving (Show, Eq
#if __GLASGOW_HASKELL__ < 710
    ,Typeable
#endif
    )

instance Exception AsyncCancelled where
#if __GLASGOW_HASKELL__ >= 708
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException
#endif

-- | Cancel an asynchronous action
--
-- This is a variant of `cancel`, but it is not interruptible.
{-# INLINE uninterruptibleCancel #-}
uninterruptibleCancel :: Async a -> IO ()
uninterruptibleCancel = uninterruptibleMask_ . cancel

-- | Cancel an asynchronous action by throwing the supplied exception
-- to it.
--
-- > cancelWith a x = throwTo (asyncThreadId a) x
--
-- The notes about the synchronous nature of 'cancel' also apply to
-- 'cancelWith'.
cancelWith :: Exception e => Async a -> e -> IO ()
cancelWith a@(Async t _) e = throwTo t e <* waitCatch a

-- | Wait for any of the supplied asynchronous operations to complete.
-- The value returned is a pair of the 'Async' that completed, and the
-- result that would be returned by 'wait' on that 'Async'.
--
-- If multiple 'Async's complete or have completed, then the value
-- returned corresponds to the first completed 'Async' in the list.
--
{-# INLINE waitAnyCatch #-}
waitAnyCatch :: [Async a] -> IO (Async a, Either SomeException a)
waitAnyCatch = atomically . waitAnyCatchSTM

-- | A version of 'waitAnyCatch' that can be used inside an STM transaction.
--
-- @since 2.1.0
waitAnyCatchSTM :: [Async a] -> STM (Async a, Either SomeException a)
waitAnyCatchSTM asyncs =
    foldr orElse retry $
      map (\a -> do r <- waitCatchSTM a; return (a, r)) asyncs

-- | Like 'waitAnyCatch', but also cancels the other asynchronous
-- operations as soon as one has completed.
--
waitAnyCatchCancel :: [Async a] -> IO (Async a, Either SomeException a)
waitAnyCatchCancel asyncs =
  waitAnyCatch asyncs `finally` mapM_ cancel asyncs

-- | Wait for any of the supplied @Async@s to complete.  If the first
-- to complete throws an exception, then that exception is re-thrown
-- by 'waitAny'.
--
-- If multiple 'Async's complete or have completed, then the value
-- returned corresponds to the first completed 'Async' in the list.
--
{-# INLINE waitAny #-}
waitAny :: [Async a] -> IO (Async a, a)
waitAny = atomically . waitAnySTM

-- | A version of 'waitAny' that can be used inside an STM transaction.
--
-- @since 2.1.0
waitAnySTM :: [Async a] -> STM (Async a, a)
waitAnySTM asyncs =
    foldr orElse retry $
      map (\a -> do r <- waitSTM a; return (a, r)) asyncs

-- | Like 'waitAny', but also cancels the other asynchronous
-- operations as soon as one has completed.
--
waitAnyCancel :: [Async a] -> IO (Async a, a)
waitAnyCancel asyncs =
  waitAny asyncs `finally` mapM_ cancel asyncs

-- | Wait for the first of two @Async@s to finish.
{-# INLINE waitEitherCatch #-}
waitEitherCatch :: Async a -> Async b
                -> IO (Either (Either SomeException a)
                              (Either SomeException b))
waitEitherCatch left right =
  tryAgain $ atomically (waitEitherCatchSTM left right)
  where
    -- See: https://github.com/simonmar/async/issues/14
    tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f

-- | A version of 'waitEitherCatch' that can be used inside an STM transaction.
--
-- @since 2.1.0
waitEitherCatchSTM :: Async a -> Async b
                -> STM (Either (Either SomeException a)
                               (Either SomeException b))
waitEitherCatchSTM left right =
    (Left  <$> waitCatchSTM left)
      `orElse`
    (Right <$> waitCatchSTM right)

-- | Like 'waitEitherCatch', but also 'cancel's both @Async@s before
-- returning.
--
waitEitherCatchCancel :: Async a -> Async b
                      -> IO (Either (Either SomeException a)
                                    (Either SomeException b))
waitEitherCatchCancel left right =
  waitEitherCatch left right `finally` (cancel left >> cancel right)

-- | Wait for the first of two @Async@s to finish.  If the @Async@
-- that finished first raised an exception, then the exception is
-- re-thrown by 'waitEither'.
--
{-# INLINE waitEither #-}
waitEither :: Async a -> Async b -> IO (Either a b)
waitEither left right = atomically (waitEitherSTM left right)

-- | A version of 'waitEither' that can be used inside an STM transaction.
--
-- @since 2.1.0
waitEitherSTM :: Async a -> Async b -> STM (Either a b)
waitEitherSTM left right =
    (Left  <$> waitSTM left)
      `orElse`
    (Right <$> waitSTM right)

-- | Like 'waitEither', but the result is ignored.
--
{-# INLINE waitEither_ #-}
waitEither_ :: Async a -> Async b -> IO ()
waitEither_ left right = atomically (waitEitherSTM_ left right)

-- | A version of 'waitEither_' that can be used inside an STM transaction.
--
-- @since 2.1.0
waitEitherSTM_:: Async a -> Async b -> STM ()
waitEitherSTM_ left right =
    (void $ waitSTM left)
      `orElse`
    (void $ waitSTM right)

-- | Like 'waitEither', but also 'cancel's both @Async@s before
-- returning.
--
waitEitherCancel :: Async a -> Async b -> IO (Either a b)
waitEitherCancel left right =
  waitEither left right `finally` (cancel left >> cancel right)

-- | Waits for both @Async@s to finish, but if either of them throws
-- an exception before they have both finished, then the exception is
-- re-thrown by 'waitBoth'.
--
{-# INLINE waitBoth #-}
waitBoth :: Async a -> Async b -> IO (a,b)
waitBoth left right = tryAgain $ atomically (waitBothSTM left right)
  where
    -- See: https://github.com/simonmar/async/issues/14
    tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f

-- | A version of 'waitBoth' that can be used inside an STM transaction.
--
-- @since 2.1.0
waitBothSTM :: Async a -> Async b -> STM (a,b)
waitBothSTM left right = do
    a <- waitSTM left
           `orElse`
         (waitSTM right >> retry)
    b <- waitSTM right
    return (a,b)


-- -----------------------------------------------------------------------------
-- Linking threads

data ExceptionInLinkedThread =
  forall a . ExceptionInLinkedThread (Async a) SomeException
#if __GLASGOW_HASKELL__ < 710
  deriving Typeable
#endif

instance Show ExceptionInLinkedThread where
  showsPrec p (ExceptionInLinkedThread (Async t _) e) =
    showParen (p >= 11) $
      showString "ExceptionInLinkedThread " .
      showsPrec 11 t .
      showString " " .
      showsPrec 11 e

instance Exception ExceptionInLinkedThread where
#if __GLASGOW_HASKELL__ >= 708
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException
#endif

-- | Link the given @Async@ to the current thread, such that if the
-- @Async@ raises an exception, that exception will be re-thrown in
-- the current thread, wrapped in 'ExceptionInLinkedThread'.
--
-- 'link' ignores 'AsyncCancelled' exceptions thrown in the other thread,
-- so that it's safe to 'cancel' a thread you're linked to.  If you want
-- different behaviour, use 'linkOnly'.
--
link :: Async a -> IO ()
link = linkOnly (not . isCancel)

-- | Link the given @Async@ to the current thread, such that if the
-- @Async@ raises an exception, that exception will be re-thrown in
-- the current thread, wrapped in 'ExceptionInLinkedThread'.
--
-- The supplied predicate determines which exceptions in the target
-- thread should be propagated to the source thread.
--
linkOnly
  :: (SomeException -> Bool)  -- ^ return 'True' if the exception
                              -- should be propagated, 'False'
                              -- otherwise.
  -> Async a
  -> IO ()
linkOnly shouldThrow a = do
  me <- myThreadId
  void $ forkRepeat $ do
    r <- waitCatch a
    case r of
      Left e | shouldThrow e -> throwTo me (ExceptionInLinkedThread a e)
      _otherwise -> return ()

-- | Link two @Async@s together, such that if either raises an
-- exception, the same exception is re-thrown in the other @Async@,
-- wrapped in 'ExceptionInLinkedThread'.
--
-- 'link2' ignores 'AsyncCancelled' exceptions, so that it's possible
-- to 'cancel' either thread without cancelling the other.  If you
-- want different behaviour, use 'link2Only'.
--
link2 :: Async a -> Async b -> IO ()
link2 = link2Only (not . isCancel)

-- | Link two @Async@s together, such that if either raises an
-- exception, the same exception is re-thrown in the other @Async@,
-- wrapped in 'ExceptionInLinkedThread'.
--
-- The supplied predicate determines which exceptions in the target
-- thread should be propagated to the source thread.
--
link2Only :: (SomeException -> Bool) -> Async a -> Async b -> IO ()
link2Only shouldThrow left@(Async tl _)  right@(Async tr _) =
  void $ forkRepeat $ do
    r <- waitEitherCatch left right
    case r of
      Left  (Left e) | shouldThrow e ->
        throwTo tr (ExceptionInLinkedThread left e)
      Right (Left e) | shouldThrow e ->
        throwTo tl (ExceptionInLinkedThread right e)
      _ -> return ()

isCancel :: SomeException -> Bool
isCancel e
  | Just AsyncCancelled <- fromException e = True
  | otherwise = False


-- -----------------------------------------------------------------------------

-- | Run two @IO@ actions concurrently, and return the first to
-- finish.  The loser of the race is 'cancel'led.
--
-- > race left right =
-- >   withAsync left $ \a ->
-- >   withAsync right $ \b ->
-- >   waitEither a b
--
race :: IO a -> IO b -> IO (Either a b)

-- | Like 'race', but the result is ignored.
--
race_ :: IO a -> IO b -> IO ()

-- | Run two @IO@ actions concurrently, and return both results.  If
-- either action throws an exception at any time, then the other
-- action is 'cancel'led, and the exception is re-thrown by
-- 'concurrently'.
--
-- > concurrently left right =
-- >   withAsync left $ \a ->
-- >   withAsync right $ \b ->
-- >   waitBoth a b
concurrently :: IO a -> IO b -> IO (a,b)

-- | 'concurrently', but ignore the result values
--
-- @since 2.1.1
concurrently_ :: IO a -> IO b -> IO ()

#define USE_ASYNC_VERSIONS 0

#if USE_ASYNC_VERSIONS

race left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitEither a b

race_ left right = void $ race left right

concurrently left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitBoth a b

concurrently_ left right = void $ concurrently left right

#else

-- MVar versions of race/concurrently
-- More ugly than the Async versions, but quite a bit faster.

-- race :: IO a -> IO b -> IO (Either a b)
race left right = concurrently' left right collect
  where
    collect m = do
        e <- m
        case e of
            Left ex -> throwIO ex
            Right r -> return r

-- race_ :: IO a -> IO b -> IO ()
race_ left right = void $ race left right

-- concurrently :: IO a -> IO b -> IO (a,b)
concurrently left right = concurrently' left right (collect [])
  where
    collect [Left a, Right b] _ = return (a,b)
    collect [Right b, Left a] _ = return (a,b)
    collect xs m = do
        e <- m
        case e of
            Left ex -> throwIO ex
            Right r -> collect (r:xs) m

concurrently' :: IO a -> IO b
             -> (IO (Either SomeException (Either a b)) -> IO r)
             -> IO r
concurrently' left right collect = do
    done <- newEmptyMVar
    mask $ \restore -> do
        -- Note: uninterruptibleMask here is because we must not allow
        -- the putMVar in the exception handler to be interrupted,
        -- otherwise the parent thread will deadlock when it waits for
        -- the thread to terminate.
        lid <- forkIO $ uninterruptibleMask_ $
          restore (left >>= putMVar done . Right . Left)
            `catchAll` (putMVar done . Left)
        rid <- forkIO $ uninterruptibleMask_ $
          restore (right >>= putMVar done . Right . Right)
            `catchAll` (putMVar done . Left)

        count <- newIORef (2 :: Int)
        let takeDone = do
                r <- takeMVar done      -- interruptible
                -- Decrement the counter so we know how many takes are left.
                -- Since only the parent thread is calling this, we can
                -- use non-atomic modifications.
                -- NB. do this *after* takeMVar, because takeMVar might be
                -- interrupted.
                modifyIORef count (subtract 1)
                return r

        let tryAgain f = f `catch` \BlockedIndefinitelyOnMVar -> f

            stop = do
                -- kill right before left, to match the semantics of
                -- the version using withAsync. (#27)
                uninterruptibleMask_ $ do
                  count' <- readIORef count
                  -- we only need to use killThread if there are still
                  -- children alive.  Note: forkIO here is because the
                  -- child thread could be in an uninterruptible
                  -- putMVar.
                  when (count' > 0) $
                    void $ forkIO $ do
                      throwTo rid AsyncCancelled
                      throwTo lid AsyncCancelled
                  -- ensure the children are really dead
                  replicateM_ count' (tryAgain $ takeMVar done)

        r <- collect (tryAgain $ takeDone) `onException` stop
        stop
        return r

concurrently_ left right = concurrently' left right (collect 0)
  where
    collect 2 _ = return ()
    collect i m = do
        e <- m
        case e of
            Left ex -> throwIO ex
            Right _ -> collect (i + 1 :: Int) m


#endif

-- | Maps an 'IO'-performing function over any 'Traversable' data
-- type, performing all the @IO@ actions concurrently, and returning
-- the original data structure with the arguments replaced by the
-- results.
--
-- If any of the actions throw an exception, then all other actions are
-- cancelled and the exception is re-thrown.
--
-- For example, @mapConcurrently@ works with lists:
--
-- > pages <- mapConcurrently getURL ["url1", "url2", "url3"]
--
-- Take into account that @async@ will try to immediately spawn a thread
-- for each element of the @Traversable@, so running this on large
-- inputs without care may lead to resource exhaustion (of memory,
-- file descriptors, or other limited resources).
mapConcurrently :: Traversable t => (a -> IO b) -> t a -> IO (t b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)

-- | `forConcurrently` is `mapConcurrently` with its arguments flipped
--
-- > pages <- forConcurrently ["url1", "url2", "url3"] $ \url -> getURL url
--
-- @since 2.1.0
forConcurrently :: Traversable t => t a -> (a -> IO b) -> IO (t b)
forConcurrently = flip mapConcurrently

-- | `mapConcurrently_` is `mapConcurrently` with the return value discarded;
-- a concurrent equivalent of 'mapM_'.
mapConcurrently_ :: F.Foldable f => (a -> IO b) -> f a -> IO ()
mapConcurrently_ f = runConcurrently . F.foldMap (Concurrently . void . f)

-- | `forConcurrently_` is `forConcurrently` with the return value discarded;
-- a concurrent equivalent of 'forM_'.
forConcurrently_ :: F.Foldable f => f a -> (a -> IO b) -> IO ()
forConcurrently_ = flip mapConcurrently_

-- | Perform the action in the given number of threads.
--
-- @since 2.1.1
replicateConcurrently :: Int -> IO a -> IO [a]
replicateConcurrently cnt = runConcurrently . sequenceA . replicate cnt . Concurrently

-- | Same as 'replicateConcurrently', but ignore the results.
--
-- @since 2.1.1
replicateConcurrently_ :: Int -> IO a -> IO ()
replicateConcurrently_ cnt = runConcurrently . F.fold . replicate cnt . Concurrently . void

-- -----------------------------------------------------------------------------

-- | A value of type @Concurrently a@ is an @IO@ operation that can be
-- composed with other @Concurrently@ values, using the @Applicative@
-- and @Alternative@ instances.
--
-- Calling @runConcurrently@ on a value of type @Concurrently a@ will
-- execute the @IO@ operations it contains concurrently, before
-- delivering the result of type @a@.
--
-- For example
--
-- > (page1, page2, page3)
-- >     <- runConcurrently $ (,,)
-- >     <$> Concurrently (getURL "url1")
-- >     <*> Concurrently (getURL "url2")
-- >     <*> Concurrently (getURL "url3")
--
newtype Concurrently a = Concurrently { runConcurrently :: IO a }

instance Functor Concurrently where
  fmap f (Concurrently a) = Concurrently $ f <$> a

instance Applicative Concurrently where
  pure = Concurrently . return
  Concurrently fs <*> Concurrently as =
    Concurrently $ (\(f, a) -> f a) <$> concurrently fs as

instance Alternative Concurrently where
  empty = Concurrently $ forever (threadDelay maxBound)
  Concurrently as <|> Concurrently bs =
    Concurrently $ either id id <$> race as bs

#if MIN_VERSION_base(4,9,0)
-- | Only defined by @async@ for @base >= 4.9@
--
-- @since 2.1.0
instance Semigroup a => Semigroup (Concurrently a) where
  (<>) = liftA2 (<>)

-- | @since 2.1.0
instance (Semigroup a, Monoid a) => Monoid (Concurrently a) where
  mempty = pure mempty
  mappend = (<>)
#else
-- | @since 2.1.0
instance Monoid a => Monoid (Concurrently a) where
  mempty = pure mempty
  mappend = liftA2 mappend
#endif

-- ----------------------------------------------------------------------------

-- | Fork a thread that runs the supplied action, and if it raises an
-- exception, re-runs the action.  The thread terminates only when the
-- action runs to completion without raising an exception.
forkRepeat :: IO a -> IO ThreadId
forkRepeat action =
  mask $ \restore ->
    let go = do r <- tryAll (restore action)
                case r of
                  Left _ -> go
                  _      -> return ()
    in forkIO go

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch

tryAll :: IO a -> IO (Either SomeException a)
tryAll = try

-- A version of forkIO that does not include the outer exception
-- handler: saves a bit of time when we will be installing our own
-- exception handler.
{-# INLINE rawForkIO #-}
rawForkIO :: IO () -> IO ThreadId
rawForkIO (IO action) = IO $ \ s ->
   case (fork# action s) of (# s1, tid #) -> (# s1, ThreadId tid #)

{-# INLINE rawForkOn #-}
rawForkOn :: Int -> IO () -> IO ThreadId
rawForkOn (I# cpu) (IO action) = IO $ \ s ->
   case (forkOn# cpu action s) of (# s1, tid #) -> (# s1, ThreadId tid #)
