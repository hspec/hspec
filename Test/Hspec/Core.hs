{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- |
-- This module contains the core types, constructors, classes, instances, and
-- utility functions common to hspec.
module Test.Hspec.Core (
  SpecTree (..)
, Example (..)
, Result (..)
, descriptions

, describe
, it
, Spec
, Specs
, UnevaluatedSpec

, quantify

-- * Deprecated types and functions
-- | The following types and functions are deprecated and will be removed with
-- the next release.
--
-- If you still need any of those, please open an issue and describe your use
-- case: <https://github.com/hspec/hspec/issues>
, AnyExample
, safeEvaluateExample
) where

import           Test.Hspec.Internal hiding (safeEvaluateExample)
import qualified Test.Hspec.Internal as Internal

{-# DEPRECATED UnevaluatedSpec "use Spec instead" #-}
type UnevaluatedSpec = Spec

-- | DEPRECATED: This is no longer needed (it's just an alias for `id` now).
descriptions :: Specs -> Specs
descriptions = id
{-# DEPRECATED descriptions "this is no longer needed, and will be removed in a future release" #-}

type Specs = [Spec]
{-# DEPRECATED AnyExample "This will be removed with the next major release.  If you still need this, raise your voice!" #-}
type AnyExample  = IO Result

{-# DEPRECATED safeEvaluateExample "This will be removed with the next major release.  If you still need this, raise your voice!" #-}
safeEvaluateExample :: AnyExample -> IO Result
safeEvaluateExample = Internal.safeEvaluateExample
