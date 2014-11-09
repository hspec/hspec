module Test.Hspec.Core.Spec (
-- * Types
  Spec
, SpecWith
, SpecM
, runSpecM
, fromSpecList

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

-- * Examples and spec items
, Example (..)
, Item (..)
, Location (..)
, LocationAccuracy(..)
, Params (..)
, ActionWith
, ProgressCallback
, Progress
, Result (..)

-- * Internal representation of a spec tree
, Tree (..)
, SpecTree
, mapSpecTree
, mapSpecItem
, mapSpecItem_
, modifyParams
, specGroup
, specItem
) where

import qualified Control.Exception as E
import           Test.Hspec.Expectations (Expectation)

import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Type

modifyParams :: (Params -> Params) -> SpecWith a -> SpecWith a
modifyParams f = mapSpecItem_ $ \item -> item {itemExample = \p -> (itemExample item) (f p)}

-- | Combine a list of specs into a larger spec.
describe :: String -> SpecWith a -> SpecWith a
describe label spec = runIO (runSpecM spec) >>= fromSpecList . return . specGroup label

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
it :: Example a => String -> a -> SpecWith (Arg a)
it label action = fromSpecList [specItem label action]

-- | An alias for `it`.
specify :: Example a => String -> a -> SpecWith (Arg a)
specify = it

-- | Run spec items of given spec in parallel.
parallel :: SpecWith a -> SpecWith a
parallel = mapSpecItem_ $ \item -> item {itemIsParallelizable = True}

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

-- | Specifies a pending example.
--
-- If you want to textually specify a behavior but do not have an example yet,
-- use this:
--
-- > describe "fancyFormatter" $ do
-- >   it "can format text in a way that everyone likes" $
-- >     pending
pending :: Expectation
pending = E.throwIO (Pending Nothing)

-- | Specifies a pending example with a reason for why it's pending.
--
-- > describe "fancyFormatter" $ do
-- >   it "can format text in a way that everyone likes" $
-- >     pendingWith "waiting for clarification from the designers"
pendingWith :: String -> Expectation
pendingWith = E.throwIO . Pending . Just
