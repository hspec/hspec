{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0)
-- Control.Concurrent.QSem is deprecated in base-4.6.0.*
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Test.Hspec.Core.Runner.Eval.Types where

import           Prelude ()
import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception         as E
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State hiding (State)
import           Data.Bifunctor
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Traversable
import           Test.Hspec.Core.Compat
import           Test.Hspec.Core.Format    (Format (..))
import           Test.Hspec.Core.Util

-- * Semaphores

data Semaphore = Semaphore {
  semaphoreWait   :: IO ()
, semaphoreSignal :: IO ()
}

instance Semigroup Semaphore where
  sa <> sb = Semaphore (semaphoreWait sa >> semaphoreWait sb) (semaphoreSignal sb >> semaphoreSignal sa)

instance Monoid Semaphore where
  mempty = Semaphore (return ()) (return ())
  mappend = (<>)

newSem :: Int -> IO Semaphore
newSem capacity = do
  sem <- newQSem capacity
  return $ Semaphore (waitQSem sem) (signalQSem sem)

newFromMVar :: MVar() -> Semaphore
newFromMVar mvar = Semaphore (takeMVar mvar) (putMVar mvar ())

withSemaphore :: Semaphore -> IO c -> IO c
withSemaphore Semaphore{..} = E.bracket_ semaphoreWait semaphoreSignal

-- * AsyncCells

-- | Computations built on top of read-only MVars
data AsyncCell m a = AsyncCell
  { -- | Blocking read and empty the cell. Equivalent to 'takeMVar'
    asyncCellTake    :: m a
    -- | Non blocking read and empty (if successful) the cell. Equivalent to 'tryTakeMVar'.
    --   Requires judicious use. In computations with multiple MVars it almost always leads to deadlock.
  , asyncCellTryTake :: m (Maybe a)
    -- | Non blocking read the cell. Equivalent to 'tryReadMVar'
  , asyncCellTryRead :: m (Maybe a)
  }

-- | Create a 'AsyncCell' of the contents of an 'MVar'
--   Forcing the 'AsyncCell' will empty the 'MVar'
asyncCellFromMVar :: MonadIO m => MVar a -> AsyncCell m a
asyncCellFromMVar mvar = AsyncCell (liftIO $ takeMVar mvar) (liftIO $ tryTakeMVar mvar) (liftIO $ tryReadMVar mvar)

-- | Like 'asyncCellFromMVar', but 'asyncCellTake' and 'asyncCellTryTake' never empty the cell
asyncCellFromMVarAlwaysRead :: MonadIO m => MVar a -> AsyncCell m a
asyncCellFromMVarAlwaysRead mvar = AsyncCell (liftIO $ readMVar mvar) (liftIO $ tryReadMVar mvar) (liftIO $ tryReadMVar mvar)

asyncCellHoist :: (forall a. f a -> g a) -> AsyncCell f v -> AsyncCell g v
asyncCellHoist hoist p = AsyncCell (hoist $ asyncCellTake p) (hoist $ asyncCellTryTake p) (hoist $ asyncCellTryRead p)

unsafeAsyncCell :: m a -> m (Maybe a) -> AsyncCell m a
unsafeAsyncCell ma mb_a = AsyncCell ma mb_a mb_a

instance Functor m => Functor (AsyncCell m) where
  fmap f (AsyncCell take_ tryTake tryRead) =
    AsyncCell (fmap f take_) ((fmap . fmap) f tryTake) ((fmap . fmap) f tryRead)

instance Applicative m => Applicative (AsyncCell m) where
  pure x = AsyncCell (pure x) (pure $ pure x) (pure $ pure x)
  pf <*> px =
    AsyncCell
      (asyncCellTake pf <*> asyncCellTake px)
      (getCompose $ Compose (asyncCellTryTake pf) <*> Compose (asyncCellTryTake px))
      (getCompose $ Compose (asyncCellTryRead pf) <*> Compose (asyncCellTryRead px))

instance Monad m => Monad (AsyncCell m) where
  return x = AsyncCell (return x) (return $ return x) (return $ return x)
  pf >>= k =
    AsyncCell
      (asyncCellTake pf >>= asyncCellTake . k)
      (asyncCellTryTake pf >>= maybe (return Nothing) (asyncCellTryTake . k))
      (asyncCellTryRead pf >>= maybe (return Nothing) (asyncCellTryRead . k))

instance MonadTrans AsyncCell where
  lift m = AsyncCell m (Just `liftM` m) (Just `liftM` m)

instance MonadIO m => MonadIO (AsyncCell m) where
  liftIO m = AsyncCell (liftIO m) (Just `liftM` liftIO m) (Just `liftM` liftIO m)

-- * Progress monoid

data Parallel p a = Partial !p | Return !p !a

isReturn :: Parallel p a -> Bool
isReturn Return {} = True
isReturn _         = False

getPartial :: Parallel p a -> Maybe p
getPartial (Partial p)  = Just p
getPartial (Return p _) = Just p

instance Functor (Parallel p) where fmap = fmapDefault
instance Foldable (Parallel p) where foldMap = foldMapDefault
instance Traversable (Parallel p) where
  traverse _ (Partial p)= pure $ Partial p
  traverse f (Return p x) = Return p <$> f x
instance Bifunctor Parallel where
  bimap l _ (Partial p)  = Partial (l p)
  bimap l r (Return p a) = Return (l p) (r a)
instance (Semigroup p, Semigroup a) => Semigroup (Parallel p a) where
  Partial p1 <> Partial p2 = Partial (p1 <> p2)
  Return p1 x <> Return p2 y = Return (p1 <> p2) (x <> y)
  Partial p1 <> Return p2 x = Return (p1 <> p2) x
  Return p1 x <> Partial p2 = Return (p1 <> p2) x
instance (Monoid p, Semigroup p, Semigroup a) => Monoid (Parallel p a) where
  mempty = Partial mempty
  mappend = (<>)

-- * Eval monad

data EvalConfig m = EvalConfig {
  evalConfigFormat          :: Format m
, evalConfigConcurrentJobs  :: Int
, evalConfigFastFail        :: Bool
, evalConfigAsyncFormatting :: Bool
}

data State m = State {
  stateConfig       :: EvalConfig m
, stateSuccessCount :: Int
, statePendingCount :: Int
, stateFailures     :: [Path]
}

newtype EvalM m a = EvalM (StateT (State m) m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (a ~ (), Monad m) => Semigroup (EvalM m a) where
 (<>) = (>>)

instance (a ~ (), Monad m) => Monoid (EvalM m a) where
  mempty = return ()
  mappend = (<>)

instance MonadTrans EvalM where
  lift = EvalM . lift
