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
, Result (..)

-- * A writer monad for constructing specs
, SpecM
, runSpecM
, fromSpecList

-- * Internal representation of a spec tree
, SpecTree (..)
, Item (..)
, mapSpecItem
, modifyParams
, describe
, it
) where

import           Test.Hspec.Core.Type

modifyParams :: (Params -> Params) -> Spec -> Spec
modifyParams f = mapSpecItem $ \item -> item {itemExample = \p -> (itemExample item) (f p)}
