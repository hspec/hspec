-- |
-- Stability: stable
--
-- Hspec is a testing framework for Haskell.
--
-- This is the library reference for Hspec.
-- The <http://hspec.github.io/ User's Manual> contains more in-depth
-- documentation.
module Test.Hspec (
-- * Types
  Spec
, Example

-- * Setting expectations
, module Test.Hspec.Expectations

-- * Defining a spec
, describe
, context
, it
, specify
, example
, pending
, pendingWith
, before
, beforeAll
, after
, afterAll
, around
, parallel
, runIO

-- * Running a spec
, hspec
) where

import           Control.Exception (finally)
import           Control.Concurrent.MVar

import           Test.Hspec.Core.Type hiding (describe, it)
import           Test.Hspec.Runner
import           Test.Hspec.HUnit ()
import           Test.Hspec.Expectations
import qualified Test.Hspec.Core as Core

-- | Combine a list of specs into a larger spec.
describe :: String -> Spec -> Spec
describe label spec = runIO (runSpecM spec) >>= fromSpecList . return . Core.describe label

-- | An alias for `describe`.
context :: String -> Spec -> Spec
context = describe

-- | Create a spec item.
--
-- A spec item consists of:
--
-- * a textual description of a desired behavior
--
-- * an example for that behavior
--
-- > describe "absolute" $ do
-- >   it "returns a positive number when given a negative number" $
-- >     absolute (-1) == 1
it :: Example a => String -> a -> Spec
it label action = fromSpecList [Core.it label action]

-- | An alias for `it`.
specify :: Example a => String -> a -> Spec
specify = it

-- | This is a type restricted version of `id`.  It can be used to get better
-- error messages on type mismatches.
--
-- Compare e.g.
--
-- > it "exposes some behavior" $ example $ do
-- >   putStrLn
--
-- with
--
-- > it "exposes some behavior" $ do
-- >   putStrLn
example :: Expectation -> Expectation
example = id

-- | Run spec items of given `Spec` in parallel.
parallel :: Spec -> Spec
parallel = mapSpecItem $ \item -> item {itemIsParallelizable = True}

-- | Run a custom action before every spec item.
before :: IO () -> Spec -> Spec
before action = around (action >>)

-- | Run a custom action before the first spec item.
beforeAll :: IO () -> Spec -> Spec
beforeAll action = fromSpecList . return . BuildSpecs . go
  where
    go spec = do
      mvar <- newMVar Nothing
      let action_ = memoize mvar action
      runSpecM $ before action_ spec

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
afterAll action = fromSpecList . return . SpecWithCleanup action . BuildSpecs . runSpecM

-- | Run a custom action before and/or after every spec item.
around :: (IO () -> IO ()) -> Spec -> Spec
around a2 = mapSpecItem $ \item -> item {itemExample = \params a1 -> itemExample item params (a1 . a2)}
