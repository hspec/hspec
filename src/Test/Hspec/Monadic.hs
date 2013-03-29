{-# OPTIONS_HADDOCK not-home #-}
module Test.Hspec.Monadic {-# DEPRECATED "use \"Test.Hspec\", \"Test.Hspec.Runner\" or \"Test.Hspec.Core\" instead" #-} (
-- * Types
  Spec
, Example

-- * Defining a spec
, describe
, context
, it
, pending

-- * Running a spec
, hspec
, Summary (..)

-- * Interface to the non-monadic API
, runSpecM
, fromSpecList

-- * Deprecated types and functions
, Specs
, descriptions
, hspecB
, hspecX
, hHspec
) where

import           System.IO
import           Control.Applicative

import           Test.Hspec.Core (runSpecM, fromSpecList)
import           Test.Hspec.Runner
import           Test.Hspec

{-# DEPRECATED Specs "use `Spec` instead" #-}             -- since 1.2.0
type Specs = Spec

{-# DEPRECATED descriptions "use `sequence_` instead" #-} -- since 1.0.0
descriptions :: [Spec] -> Spec
descriptions = sequence_

{-# DEPRECATED hspecX "use `hspec` instead" #-}           -- since 1.2.0
hspecX :: Spec -> IO ()
hspecX = hspec

{-# DEPRECATED hspecB "use `hspecWith` instead" #-}       -- since 1.4.0
hspecB :: Spec -> IO Bool
hspecB spec = (== 0) . summaryFailures <$> hspecWith defaultConfig spec

{-# DEPRECATED hHspec "use hspecWith instead" #-}         -- since 1.4.0
hHspec :: Handle -> Spec -> IO Summary
hHspec h = hspecWith defaultConfig {configHandle = h}
