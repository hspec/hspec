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
, hspecX
, hHspec
) where

import           Test.Hspec.Internal
import           Test.Hspec.Pending
import           Test.Hspec.Runner
