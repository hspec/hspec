{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- |
-- Stability: unstable
--
-- This module provides access to Hspec's internals.  It is less stable than
-- other parts of the API. For most users @Test.Hspec@ is more suitable!
module Test.Hspec.Core.Spec (

-- * Defining a spec
  it
, specify
, describe
, context
, pending
, pendingWith
, xit
, xspecify
, xdescribe
, xcontext

, focus
, fit
, fspecify
, fdescribe
, fcontext

, parallel
, sequential

-- * The @SpecM@ monad
, module Test.Hspec.Core.Spec.Monad

-- * A type class for examples
, module Test.Hspec.Core.Example

-- * Internal representation of a spec tree
, module Test.Hspec.Core.Tree
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified Control.Exception as E
import           Data.CallStack

import           Test.Hspec.Expectations (Expectation)

import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Hooks
import           Test.Hspec.Core.Tree
import           Test.Hspec.Core.Spec.Monad

-- | The @describe@ function combines a list of specs into a larger spec.
describe :: HasCallStack => String -> SpecWith a -> SpecWith a
describe label spec = runIO (runSpecM spec) >>= fromSpecList . return . specGroup label

-- | @context@ is an alias for `describe`.
context :: HasCallStack => String -> SpecWith a -> SpecWith a
context = describe

-- |
-- Changing `describe` to `xdescribe` marks all spec items of the corresponding subtree as pending.
--
-- This can be used to temporarily disable spec items.
xdescribe :: HasCallStack => String -> SpecWith a -> SpecWith a
xdescribe label spec = before_ pending_ $ describe label spec

-- | @xcontext@ is an alias for `xdescribe`.
xcontext :: HasCallStack => String -> SpecWith a -> SpecWith a
xcontext = xdescribe

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
it :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
it label action = fromSpecList [specItem label action]

-- | @specify@ is an alias for `it`.
specify :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
specify = it

-- |
-- Changing `it` to `xit` marks the corresponding spec item as pending.
--
-- This can be used to temporarily disable a spec item.
xit :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
xit label action = before_ pending_ $ it label action

-- | @xspecify@ is an alias for `xit`.
xspecify :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
xspecify = xit

-- | `focus` focuses all spec items of the given spec.
--
-- Applying `focus` to a spec with focused spec items has no effect.
focus :: SpecWith a -> SpecWith a
focus spec = do
  xs <- runIO (runSpecM spec)
  let
    ys
      | any (any itemIsFocused) xs = xs
      | otherwise = map (bimapTree id (\ item -> item {itemIsFocused = True})) xs
  fromSpecList ys

-- | @fit@ is an alias for @fmap focus . it@
fit :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
fit = fmap focus . it

-- | @fspecify@ is an alias for `fit`.
fspecify :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
fspecify = fit

-- | @fdescribe@ is an alias for @fmap focus . describe@
fdescribe :: HasCallStack => String -> SpecWith a -> SpecWith a
fdescribe = fmap focus . describe

-- | @fcontext@ is an alias for `fdescribe`.
fcontext :: HasCallStack => String -> SpecWith a -> SpecWith a
fcontext = fdescribe

-- | `parallel` marks all spec items of the given spec to be safe for parallel
-- evaluation.
parallel :: SpecWith a -> SpecWith a
parallel = mapSpecItem_ (setParallelizable True)

-- | `sequential` marks all spec items of the given spec to be evaluated sequentially.
sequential :: SpecWith a -> SpecWith a
sequential = mapSpecItem_ (setParallelizable False)

setParallelizable :: Bool -> Item a -> Item a
setParallelizable value item = item {itemIsParallelizable = itemIsParallelizable item <|> Just value}

-- | `pending` can be used to mark a spec item as pending.
--
-- If you want to textually specify a behavior but do not have an example yet,
-- use this:
--
-- > describe "fancyFormatter" $ do
-- >   it "can format text in a way that everyone likes" $
-- >     pending
pending :: HasCallStack => Expectation
pending = E.throwIO (Pending location Nothing)

pending_ :: Expectation
pending_ = (E.throwIO (Pending Nothing Nothing))

-- |
-- `pendingWith` is similar to `pending`, but it takes an additional string
-- argument that can be used to specify the reason for why the spec item is pending.
pendingWith :: HasCallStack => String -> Expectation
pendingWith = E.throwIO . Pending location . Just
