{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
, hspecWith
, hspecB
, Summary (..)
, Config (..)
, defaultConfig

-- * Interface to the non-monadic API
, runSpecM
, fromSpecList

-- * Deprecated types and functions
, Specs
, descriptions
, hspecX
, hHspec
) where

import           System.IO
import           Control.Monad.Trans.Writer (tell)

import           Test.Hspec.Internal hiding (describe, it)
import qualified Test.Hspec.Core as Core
import qualified Test.Hspec.Runner as Runner
import           Test.Hspec.Runner (Summary (..))
import           Test.Hspec.Config
import           Test.Hspec.Pending (Pending)
import qualified Test.Hspec.Pending as Pending

-- | Create a document of the given spec and write it to stdout.
--
-- Exit the program with `System.Exit.exitFailure` if at least one example fails.
--
-- (see also `hspecWith`)
hspec :: Spec -> IO ()
hspec = Runner.hspec . runSpecM

-- | Run a spec.  This is similar to `hspec`, but more flexible.
hspecWith :: Config -> Spec -> IO Summary
hspecWith c = Runner.hspecWith c . runSpecM

-- | Create a document of the given spec and write it to stdout.
--
-- Return `True` if all examples passed, `False` otherwise.
hspecB :: Spec -> IO Bool
hspecB = Runner.hspecB . runSpecM

-- | Create a `Spec` from a forest of `SpecTree`s.
fromSpecList :: [SpecTree] -> Spec
fromSpecList = SpecM . tell

-- | The @describe@ function combines a list of specs into a larger spec.
describe :: String -> Spec -> Spec
describe label action = SpecM . tell $ [Core.describe label (runSpecM action)]

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
it label action = (SpecM . tell) [Core.it label action]

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

{-# DEPRECATED Specs "use Spec instead" #-}
type Specs = SpecM ()

{-# DEPRECATED descriptions "use sequence_ instead" #-}
descriptions :: [Spec] -> Spec
descriptions = sequence_

{-# DEPRECATED hspecX "use hspec instead" #-}
hspecX :: Spec -> IO a
hspecX = Runner.hspecX . runSpecM

{-# DEPRECATED hHspec "use hspecWith instead" #-}
hHspec :: Handle -> Spec -> IO Summary
hHspec h = Runner.hHspec h . runSpecM
