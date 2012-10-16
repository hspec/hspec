-- |
-- This module provides Hspec's core primitives.  It is less stable than other
-- parts of the API.  For most use cases "Test.Hspec" is more suitable.
module Test.Hspec.Core (
-- * Types
  SpecTree
, Specs
, Example (..)
, Result (..)
, Pending

-- * Defining a spec
, describe
, it
, pending

-- * Running a spec
, hspec
, hspecB
, Summary (..)

-- * Deprecated types and functions
, Spec
, hspecX
, hHspec
) where

import           Test.Hspec.Internal
import           Test.Hspec.Pending
import           Test.Hspec.Runner

-- {-# DEPRECATED Spec "use `SpecTree` instead" #-}
type Spec = SpecTree
