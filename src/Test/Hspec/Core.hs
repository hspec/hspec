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
, SpecTree (..)
, mapSpecTree
, Item (..)
, Location (..)
, LocationAccuracy(..)
, mapSpecItem
, modifyParams
, specGroup
, specItem

-- * Deprecated functions
, describe
, it
) where

import           Test.Hspec.Core.Type

modifyParams :: (Params -> Params) -> Spec -> Spec
modifyParams f = mapSpecItem $ \item -> item {itemExample = \p -> (itemExample item) (f p)}

{-# DEPRECATED describe "use `specGroup` instead" #-}
describe :: String -> [SpecTree] -> SpecTree
describe = specGroup

{-# DEPRECATED it "use `specItem` instead" #-}
it :: Example a => String -> a -> SpecTree
it = specItem
