module Test.Hspec.Core (
  SpecTree
, Example (..)
, Result (..)
, pending
, descriptions

, describe
, it
, Spec
, UnevaluatedSpec
, EvaluatedSpec
, Pending

, quantify
) where

import           Test.Hspec.Internal

{-# DEPRECATED UnevaluatedSpec "use Spec instead" #-}
type UnevaluatedSpec = Spec

-- | DEPRECATED: This is no longer needed (it's just an alias for `id` now).
descriptions :: Specs -> Specs
descriptions = id
{-# DEPRECATED descriptions "this is no longer needed, and will be removed in a future release" #-}
