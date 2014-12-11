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
, SpecWith
, Arg
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
, parallel
, runIO

-- * Hooks
, ActionWith
, before
, before_
, beforeWith
, beforeAll
, after
, after_
, afterAll
, afterAll_
, around
, around_
, aroundWith

-- * Running a spec
, hspec
) where

import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Hooks
import           Test.Hspec.Runner
import           Test.Hspec.Expectations

-- | @example@ is a type restricted version of `id`.  It can be used to get better
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

-- | @context@ is an alias for `describe`.
context :: String -> SpecWith a -> SpecWith a
context = describe

-- | @specify@ is an alias for `it`.
specify :: Example a => String -> a -> SpecWith (Arg a)
specify = it
