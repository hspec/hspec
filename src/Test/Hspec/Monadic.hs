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
import           Control.Monad.Trans.Writer (tell)

import           Test.Hspec.Internal hiding (describe, it)
import qualified Test.Hspec.Internal as Internal
import           Test.Hspec.Runner
import           Test.Hspec.Pending (Pending)
import qualified Test.Hspec.Pending as Pending

-- | Create a `Spec` from a forest of `SpecTree`s.
fromSpecList :: [SpecTree] -> Spec
fromSpecList = SpecM . tell

-- | The @describe@ function combines a list of specs into a larger spec.
describe :: String -> Spec -> Spec
describe label action = SpecM . tell $ [Internal.describe label (runSpecM action)]

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
it label action = (SpecM . tell) [Internal.it label action]

-- | A pending example.
--
-- If you want to report on a behavior but don't have an example yet, use this.
--
-- > describe "fancyFormatter" $ do
-- >   it "can format text in a way that everyone likes" $
-- >     pending
--
-- You can give an optional reason for why it's pending.
--
-- > describe "fancyFormatter" $ do
-- >   it "can format text in a way that everyone likes" $
-- >     pending "waiting for clarification from the designers"
pending :: String  -> Pending
pending = Pending.pending

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
