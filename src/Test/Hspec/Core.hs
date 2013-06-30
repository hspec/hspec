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
, describe
, it

-- * Deprecated types and functions
, Spec
, Specs
, hspecB
, hspecX
, hHspec
, hspec
) where

import           Control.Applicative
import           System.IO (Handle)

import           Test.Hspec.Core.Type hiding (Spec)
import qualified Test.Hspec.Runner as Runner
import           Test.Hspec.Runner (Summary(..), Config(..), defaultConfig)

hspecWith :: Config -> [SpecTree] -> IO Summary
hspecWith c = Runner.hspecWith c . fromSpecList

{-# DEPRECATED hspecX "use `Test.Hspec.Runner.hspec` instead" #-}     -- since 1.2.0
hspecX :: [SpecTree] -> IO ()
hspecX = hspec

{-# DEPRECATED hspec "use `Test.Hspec.Runner.hspec` instead" #-}      -- since 1.4.0
hspec :: [SpecTree] -> IO ()
hspec = Runner.hspec . fromSpecList

{-# DEPRECATED hspecB "use `Test.Hspec.Runner.hspecWith` instead" #-} -- since 1.4.0
hspecB :: [SpecTree] -> IO Bool
hspecB spec = (== 0) . summaryFailures <$> hspecWith defaultConfig spec

{-# DEPRECATED hHspec "use `Test.Hspec.Runner.hspecWith` instead" #-} -- since 1.4.0
hHspec :: Handle -> [SpecTree] -> IO Summary
hHspec h = hspecWith defaultConfig {configHandle = Left h}

{-# DEPRECATED Spec "use `SpecTree` instead" #-}                      -- since 1.4.0
type Spec = SpecTree

{-# DEPRECATED Specs "use `[SpecTree]` instead" #-}                   -- since 1.4.0
type Specs = [SpecTree]
