module Test.Hspec.Core.Hooks (
  before
, beforeAll
, after
, afterAll
, around
) where

import           Control.Exception (finally)
import           Control.Concurrent.MVar

import           Test.Hspec.Core.Type hiding (describe, it)
import           Test.Hspec.HUnit ()

-- | Run a custom action before every spec item.
before :: IO () -> Spec -> Spec
before action = around (action >>)

-- | Run a custom action before the first spec item.
beforeAll :: IO () -> Spec -> Spec
beforeAll action spec = do
  mvar <- runIO (newMVar Nothing)
  let action_ = memoize mvar action
  before action_ spec

memoize :: MVar (Maybe a) -> IO a -> IO a
memoize mvar action = modifyMVar mvar $ \ma -> case ma of
  Just a -> return (ma, a)
  Nothing -> do
    a <- action
    return (Just a, a)

-- | Run a custom action after every spec item.
after :: IO () -> Spec -> Spec
after action = around (`finally` action)

-- | Run a custom action after the last spec item.
afterAll :: IO () -> Spec -> Spec
afterAll action spec = runIO (runSpecM spec) >>= fromSpecList . return . SpecWithCleanup action

-- | Run a custom action before and/or after every spec item.
around :: (IO () -> IO ()) -> Spec -> Spec
around a2 = mapSpecItem $ \item -> item {itemExample = \params a1 -> itemExample item params (a1 . a2)}
