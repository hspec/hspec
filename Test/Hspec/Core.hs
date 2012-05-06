-- |
-- This module contains the core types, constructors, classes, instances, and
-- utility functions common to hspec.
module Test.Hspec.Core (
  SpecTree
, Example (..)
, Result (..)
, pending
, descriptions

, describe
, it
, Spec
, Specs
, UnevaluatedSpec
, EvaluatedSpec
, Pending

, quantify
, success
, failure
, isFailure
, failedCount
) where

import           Test.Hspec.Internal

{-# DEPRECATED UnevaluatedSpec "use Spec instead" #-}
type UnevaluatedSpec = Spec

-- | DEPRECATED: This is no longer needed (it's just an alias for `id` now).
descriptions :: Specs -> Specs
descriptions = id
{-# DEPRECATED descriptions "this is no longer needed, and will be removed in a future release" #-}

type Specs = [Spec]

type EvaluatedSpec = SpecTree Result

success :: [EvaluatedSpec] -> Bool
success = not . failure

failure :: [EvaluatedSpec] -> Bool
failure = any p
  where
    p (SpecGroup _ xs) = any p xs
    p (SpecExample _ x) = isFailure x

isFailure :: Result -> Bool
isFailure (Fail _) = True
isFailure _        = False

failedCount :: [EvaluatedSpec] -> Int
failedCount = sum . map count
  where
    count (SpecGroup _ xs) = sum (map count xs)
    count (SpecExample _ x) = if isFailure x then 1 else 0
