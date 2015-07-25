{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,8,1)
#define HAS_SOURCE_LOCATIONS
{-# LANGUAGE ImplicitParams #-}
#endif
-- |
-- Stability: unstable
--
-- This module provides access to Hspec's internals.  It is less stable than
-- other parts of the API. For most users @Test.Hspec@ is more suitable!
module Test.Hspec.Core.Spec (

-- * Defining a spec
  describe
, it
, pending
, pendingWith
, parallel

-- * The @SpecM@ monad
, module Test.Hspec.Core.Spec.Monad

-- * A type class for examples
, module Test.Hspec.Core.Example

-- * Internal representation of a spec tree
, module Test.Hspec.Core.Tree
) where

#ifdef HAS_SOURCE_LOCATIONS
import           GHC.Stack
#endif

import qualified Control.Exception as E

import           Test.Hspec.Expectations (Expectation)

import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Tree
import           Test.Hspec.Core.Spec.Monad

-- | The @describe@ function combines a list of specs into a larger spec.
describe :: String -> SpecWith a -> SpecWith a
describe label spec = runIO (runSpecM spec) >>= fromSpecList . return . specGroup label

-- | The @it@ function creates a spec item.
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
#ifdef HAS_SOURCE_LOCATIONS
it :: (?loc :: CallStack, Example a) => String -> a -> SpecWith (Arg a)
#else
it :: Example a => String -> a -> SpecWith (Arg a)
#endif
it label action = fromSpecList [specItem label action]

-- | `parallel` marks all spec items of the given spec to be safe for parallel
-- evaluation.
parallel :: SpecWith a -> SpecWith a
parallel = mapSpecItem_ $ \item -> item {itemIsParallelizable = True}

-- | `pending` can be used to indicate that an example is /pending/.
--
-- If you want to textually specify a behavior but do not have an example yet,
-- use this:
--
-- > describe "fancyFormatter" $ do
-- >   it "can format text in a way that everyone likes" $
-- >     pending
pending :: Expectation
pending = E.throwIO (Pending Nothing)

-- |
-- `pendingWith` is similar to `pending`, but it takes an additional string
-- argument that can be used to specify the reason for why it's pending.
pendingWith :: String -> Expectation
pendingWith = E.throwIO . Pending . Just
