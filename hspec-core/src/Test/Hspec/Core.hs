-- |
-- Stability: experimental
--
-- This module provides access to Hspec's internals.  It is less stable than
-- other parts of the API.  For most users "Test.Hspec" is more suitable!
module Test.Hspec.Core (

-- * A type class for examples
  Example (..)
, Params (..)
, Progress
, ProgressCallback
, Result (..)

-- * A writer monad for constructing specs
, SpecM
, runSpecM
, fromSpecList

-- * Internal representation of a spec tree
, Tree (..)
, SpecTree
, mapSpecTree
, Item (..)
, Location (..)
, LocationAccuracy(..)
, ActionWith
, mapSpecItem
, mapSpecItem_
, modifyParams
, describe
, it
) where

import           Test.Hspec.Core.Type

modifyParams :: (Params -> Params) -> SpecWith a -> SpecWith a
modifyParams f = mapSpecItem_ $ \item -> item {itemExample = \p -> (itemExample item) (f p)}

-- | The @describe@ function combines a list of specs into a larger spec.
describe :: String -> [SpecTree a] -> SpecTree a
describe = specGroup

-- | Create a spec item.
it :: Example a => String -> a -> SpecTree (Arg a)
it = specItem
