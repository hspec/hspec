{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Test.Hspec.Contrib.Mocks.V1 (
  stubAction
, withSpy
) where

import           Test.HUnit
import           Data.CallStack (HasCallStack)
import           Data.IORef

#if !MIN_VERSION_base(4,6,0)
atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' = atomicModifyIORef
#endif

-- | Create a [test stub](https://en.wikipedia.org/wiki/Test_stub) action.
--
-- >>> stub <- stubAction ["foo", "bar", "baz"]
-- >>> stub
-- "foo"
-- >>> stub
-- "bar"
-- >>> stub
-- "baz"
-- >>> stub
-- *** Exception: HUnitFailure ...stubAction: no values left...
--
-- @since 0.5.2
stubAction :: HasCallStack => [a] -> IO (IO a)
stubAction values = do
  ref <- newIORef values
  return $ do
    atomicModifyIORef ref takeValue >>= maybe noValuesLeft return
  where
    noValuesLeft :: IO a
    noValuesLeft = assertFailure "stubAction: no values left"

    takeValue :: [a] -> ([a], Maybe a)
    takeValue xs = case xs of
      [] -> ([], Nothing)
      a : as -> (as, Just a)

-- | Create a [test spy](https://en.wikipedia.org/wiki/Test_double) action.
--
-- Record any arguments that are passed to that action.
--
-- >>> withSpy $ \ spy -> spy "foo" >> spy "bar" >> spy "baz"
-- ["foo","bar","baz"]
--
-- @since 0.5.2
withSpy :: ((a -> IO ()) -> IO ()) -> IO [a]
withSpy action = do
  ref <- newIORef []
  action (\ x -> atomicModifyIORef' ref $ \ xs -> (x : xs, ()))
  reverse `fmap` readIORef ref
