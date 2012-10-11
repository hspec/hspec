-- |
-- NOTE: There is a monadic and a non-monadic API.  This is the documentation
-- for the non-monadic API.  The monadic API is more stable, so you may prefer
-- it over this one.  For documentation on the monadic API look at
-- "Test.Hspec".
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
, Summary (..)

-- * Internals
, quantify
, Result (..)

-- * Deprecated types and functions
, descriptions
, hspecX
, hHspec
, AnyExample
, UnevaluatedSpec
) where

import           Test.Hspec.Internal
import           Test.Hspec.Pending
import           Test.Hspec.Runner
import           Test.Hspec.Util

{-# DEPRECATED UnevaluatedSpec "use Spec instead" #-}
type UnevaluatedSpec = Spec

{-# DEPRECATED descriptions "this is no longer needed, and will be removed in a future release" #-}
descriptions :: Specs -> Specs
descriptions = id

{-# DEPRECATED AnyExample "This will be removed with the next major release.  If you still need this, raise your voice!" #-}
type AnyExample  = IO Result
