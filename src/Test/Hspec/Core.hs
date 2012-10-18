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

import           Test.Hspec.Internal hiding (Spec)
import           Test.Hspec.Pending
import           Test.Hspec.Runner hiding (hspec)
import qualified Test.Hspec.Runner as Runner
import           Test.Hspec.Monadic (fromSpecList)

hspec :: [SpecTree] -> IO ()
hspec = Runner.hspec . fromSpecList

-- | A forest of `SpecTree`s.
type Specs = [SpecTree]

-- {-# DEPRECATED Spec "use `SpecTree` instead" #-}
type Spec = SpecTree
