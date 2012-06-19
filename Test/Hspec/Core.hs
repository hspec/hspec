-- |
-- This module contains the core types, constructors, classes, instances, and
-- utility functions common to hspec.
module Test.Hspec.Core (
-- * Types
  Spec
, Specs
, Example (..)
, Pending

-- * Defining a spec
, describe
, it
, pending

-- * Running a spec
, hspec
, hspecB
, hHspec
, Summary (..)

-- * Internals
, quantify
, Result (..)

-- * Deprecated types and functions
-- | The following types and functions are deprecated and will be removed with
-- the next release.
--
-- If you still need any of those, please open an issue and describe your use
-- case: <https://github.com/hspec/hspec/issues>
, AnyExample
, safeEvaluateExample
, hspecX
, descriptions
, UnevaluatedSpec
) where

import           Test.Hspec.Internal hiding (safeEvaluateExample)
import qualified Test.Hspec.Internal as Internal
import           Test.Hspec.Pending
import           Test.Hspec.Runner

{-# DEPRECATED UnevaluatedSpec "use Spec instead" #-}
type UnevaluatedSpec = Spec

-- | DEPRECATED: This is no longer needed (it's just an alias for `id` now).
descriptions :: Specs -> Specs
descriptions = id
{-# DEPRECATED descriptions "this is no longer needed, and will be removed in a future release" #-}

{-# DEPRECATED AnyExample "This will be removed with the next major release.  If you still need this, raise your voice!" #-}
type AnyExample  = IO Result

{-# DEPRECATED safeEvaluateExample "This will be removed with the next major release.  If you still need this, raise your voice!" #-}
safeEvaluateExample :: AnyExample -> IO Result
safeEvaluateExample = Internal.safeEvaluateExample
