-- |
-- Stability: stable
--
-- Hspec is a testing framework for Haskell.
--
-- This is the library reference for Hspec.  The User's Manual is at:
--
-- <http://hspec.github.io/>
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
, example
, pending
, pendingWith
, before
, after
, around
, parallel

-- * Running a spec
, hspec
) where

import           Test.Hspec.Core.Type hiding (describe, it)
import           Test.Hspec.Runner
import           Test.Hspec.HUnit ()
import           Test.Hspec.Expectations
import           Test.Hspec.Core (mapSpecItem)
import qualified Test.Hspec.Core as Core

import           Data.IORef

-- | Combine a list of specs into a larger spec.
describe :: String -> Spec -> Spec
describe label action = fromSpecList [Core.describe label (runSpecM action)]

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

-- | Run examples of given spec in parallel.
parallel :: Spec -> Spec
parallel = mapSpecItem $ \item -> item {itemIsParallelizable = True}

-- | Run a custom action before every spec item.
before :: IO () -> Spec -> Spec
before action = around (action >>)

-- | Run a custom action after every spec item.
after :: IO () -> Spec -> Spec
after action = around (>> action)

-- | Run a custom action before and/or after every spec item.
around :: (IO () -> IO ()) -> Spec -> Spec
around action = mapSpecItem $ \item -> item {itemExample = \params -> wrap (itemExample item params)}
  where
    wrap e = do
      ref <- newIORef Success
      action (e >>= writeIORef ref)
      readIORef ref
