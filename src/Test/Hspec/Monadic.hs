module Test.Hspec.Monadic (
-- * Types
  Spec
, SpecM
, Example
, Pending

-- * Defining a spec
, describe
, context
, it
, pending

-- * Running a spec
, hspec

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

import           Test.Hspec.Core.Type hiding (describe, it)
import qualified Test.Hspec.Core.Type as Core
import           Test.Hspec.Runner
import           Test.Hspec.Pending

-- | The @describe@ function combines a list of specs into a larger spec.
describe :: String -> Spec -> Spec
describe label action = fromSpecList [Core.describe label (runSpecM action)]

-- | An alias for `describe`.
context :: String -> Spec -> Spec
context = describe

-- |
-- Create a set of specifications for a specific type being described.  Once
-- you know what you want specs for, use this.
--
-- > describe "abs" $ do
-- >   it "returns a positive number given a negative number" $
-- >     abs (-1) == 1
it :: Example v => String -> v -> Spec
it label action = fromSpecList [Core.it label action]

{-# DEPRECATED Specs "use `Spec` instead" #-}             -- since 1.2.0
type Specs = SpecM ()

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
