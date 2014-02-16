-- |
-- Stability: stable
--
-- Hspec is a testing library for Haskell.
--
-- This is the library reference for Hspec.
-- The <http://hspec.github.io/ User's Manual> contains more in-depth
-- documentation.
module Test.Hspec (
-- * Types
  Spec
, SpecWith
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
, after_
, around
, around_
, parallel

-- * Running a spec
, hspec
) where

import           Control.Exception (finally)

import           Test.Hspec.Core.Type hiding (describe, it)
import           Test.Hspec.Runner
import           Test.Hspec.HUnit ()
import           Test.Hspec.Expectations
import qualified Test.Hspec.Core as Core

-- | Combine a list of specs into a larger spec.
describe :: String -> SpecWith a -> SpecWith a
describe label action = fromSpecList [Core.describe label (runSpecM action)]

-- | An alias for `describe`.
context :: String -> SpecWith a -> SpecWith a
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
it :: Example e => String -> e -> SpecWith (Arg e)
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
parallel :: SpecWith a -> SpecWith a
parallel = mapSpecItem $ \item -> item {itemIsParallelizable = True}

-- | Run a custom action before every spec item.
before :: IO a -> SpecWith a -> Spec
before action = around (action >>=)

-- | Run a custom action after every spec item.
after :: (a -> IO ()) -> SpecWith a -> SpecWith a
after a2 = mapSpecItem $ \item -> item {itemExample = \params a1 -> itemExample item params (\f -> a1 (\a -> f a `finally` a2 a))}

-- | Run a custom action after every spec item.
after_ :: IO () -> Spec -> Spec
after_ action = after $ \() -> action

-- | Run a custom action before and/or after every spec item.
around :: ((a -> IO ()) -> IO ()) -> SpecWith a -> Spec
around a2 = mapSpecItem $ \item -> item {itemExample = \params a1 -> itemExample item params (\x -> a1 $ \() -> a2 x)}

-- | Run a custom action before and/or after every spec item.
around_ :: (IO () -> IO ()) -> Spec -> Spec
around_ action = around $ action . ($ ())
