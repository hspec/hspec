{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Test.Hspec.Core.Formatters.Free where

import           Prelude                ()
import           Control.Monad.Catch    as Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Functor.Identity
import           Test.Hspec.Core.Compat

data FreeF f a b = PureF a | FreeF (f b)
  deriving Functor

newtype FreeT f m a = FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }

type Free f = FreeT f Identity

iterT :: (Functor f, Monad m) => (f (m a) -> m a) -> FreeT f m a -> m a
iterT f (FreeT m) = do
    val <- m
    case fmap (iterT f) val of
        PureF x -> return x
        FreeF y -> f y

iterTM :: (Functor f, Monad m, MonadTrans t, Monad (t m)) => (f (t m a) -> t m a) -> FreeT f m a -> t m a
iterTM f (FreeT m) = do
    val <- lift m
    case fmap (iterTM f) val of
        PureF x -> return x
        FreeF y -> f y

foldF :: Functor f => (f res -> res) -> (a -> res) -> Free f a -> res
foldF f var = runIdentity . iterT (Identity . f . fmap runIdentity) . fmap var

instance (Functor f, Monad m) => Functor (FreeT f m) where
  fmap f (FreeT m) = FreeT (liftM f' m) where
    f' (PureF a)  = PureF (f a)
    f' (FreeF as) = FreeF (fmap (fmap f) as)

instance (Functor f, Monad m) => Applicative (FreeT f m) where
  pure a = FreeT (return (PureF a))
  (<*>) = ap

instance (Functor f, Monad m) => Monad (FreeT f m) where
  return = pure
  FreeT m >>= f = FreeT $ m >>= \v -> case v of
    PureF a -> runFreeT (f a)
    FreeF w -> return (FreeF (fmap (>>= f) w))

instance MonadTrans (FreeT f) where
  lift = FreeT . liftM PureF

instance (Functor f, MonadThrow m) => MonadThrow (FreeT f m) where
  throwM = lift . throwM

instance (Functor f, MonadCatch m) => MonadCatch (FreeT f m) where
  FreeT m `catch` f =
    FreeT $ liftM (fmap (`Monad.catch` f)) m `Monad.catch` (runFreeT . f)

instance (Functor f) => MonadIO (FreeT f IO) where
  liftIO = lift . liftIO

liftF :: (Monad m, Functor f) => f a -> FreeT f m a
liftF = FreeT . return . FreeF . fmap return
