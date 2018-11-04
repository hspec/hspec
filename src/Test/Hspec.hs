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
, it
, specify
, describe
, context
, example
, parallel
, runIO

-- * Pending spec items
-- |
-- During a test run a /pending/ spec item is:
--
-- 1. not executed
--
-- 1. reported as \"pending\"
, pending
, pendingWith
, xit
, xspecify
, xdescribe
, xcontext

-- * Focused spec items
-- |
-- During a test run, when a spec contains /focused/ spec items, all other spec
-- items are ignored.
, focus
, fit
, fspecify
, fdescribe
, fcontext

-- * Hooks
, ActionWith
, before
, before_
, beforeWith
, beforeAll
, beforeAll_
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
