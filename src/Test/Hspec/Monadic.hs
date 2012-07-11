{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Test.Hspec.Monadic (
-- * Types
  Spec
, Example
, Pending

-- * Defining a spec
, describe
, context
, it
, pending

-- * Running a spec
, hspec
, hspecB
, hHspec
, Summary (..)

-- * Interface to the non-monadic API
, runSpecM
, fromSpecList

-- deprecated stuff
, Specs
, descriptions
, hspecX
) where

import           System.IO
import           Test.Hspec.Core (Example)
import qualified Test.Hspec.Core as Core
import qualified Test.Hspec.Runner as Runner
import           Test.Hspec.Runner (Summary (..))
import           Test.Hspec.Pending (Pending)
import qualified Test.Hspec.Pending as Pending

import           Control.Monad.Trans.Writer (Writer, execWriter, tell)


type Spec = SpecM ()

newtype SpecM a = SpecM (Writer [Core.Spec] a)
  deriving Monad

-- | Create a document of the given spec and write it to stdout.
--
-- Exit the program with `exitSuccess` if all examples passed, with
-- `exitFailure` otherwise.
hspec :: Spec -> IO ()
hspec = Runner.hspec . runSpecM

-- | Create a document of the given spec and write it to stdout.
--
-- Return `True` if all examples passed, `False` otherwise.
hspecB :: Spec -> IO Bool
hspecB = Runner.hspecB . runSpecM

-- | Create a document of the given specs and write it to the given handle.
--
-- > writeReport filename specs = withFile filename WriteMode (\h -> hHspec h specs)
--
hHspec :: Handle -> Spec -> IO Summary
hHspec h = Runner.hHspec h . runSpecM

-- | Convert a monadic spec into a non-monadic spec.
runSpecM :: Spec -> [Core.Spec]
runSpecM (SpecM specs) = execWriter specs

-- | Convert a non-monadic spec into a monadic spec.
fromSpecList :: [Core.Spec] -> Spec
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
