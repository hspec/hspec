{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module contains the runners that take a set of specs, specified in a monadic style, evaluate their examples, and
-- report to a given handle.
--
-- The three functions you'll use the most are 'hspec', 'describe', and 'it'. Here is an
-- example of functions that format and unformat phone numbers and the specs for them.
--
-- > import Test.Hspec.Monadic
-- > import Test.Hspec.QuickCheck
-- > import Test.Hspec.HUnit
-- > import Test.QuickCheck hiding (property)
-- > import Test.HUnit
-- >
-- > main = hspec mySpecs
--
-- Since the specs are often used to tell you what to implement, it's best to start with
-- undefined functions. Once we have some specs, then you can implement each behavior
-- one at a time, ensuring that each behavior is met and there is no undocumented behavior.
--
-- > unformatPhoneNumber :: String -> String
-- > unformatPhoneNumber number = undefined
-- >
-- > formatPhoneNumber :: String -> String
-- > formatPhoneNumber number = undefined
--
-- The 'describe' function takes a list of behaviors and examples bound together with the 'it' function
--
-- > mySpecs = describe "unformatPhoneNumber" $ do
--
-- A boolean expression can act as a behavior's example.
--
-- >   it "removes dashes, spaces, and parenthesies" $
-- >     unformatPhoneNumber "(555) 555-1234" == "5555551234"
--
-- The 'pending' function marks a behavior as pending an example. The example doesn't count as failing.
--
-- >   it "handles non-US phone numbers" $
-- >     pending "need to look up how other cultures format phone numbers"
--
-- An HUnit 'Test' can act as a behavior's example. (must import @Test.Hspec.HUnit@)
--
-- >   it "removes the \"ext\" prefix of the extension" $ do
-- >     let expected = "5555551234135"
-- >         actual   = unformatPhoneNumber "(555) 555-1234 ext 135"
-- >     assertEqual "remove extension" expected actual
--
-- An @IO()@ action is treated like an HUnit 'TestCase'. (must import @Test.Hspec.HUnit@)
--
-- >   it "converts letters to numbers" $ do
-- >     let expected = "6862377"
-- >         actual   = unformatPhoneNumber "NUMBERS"
-- >     assertEqual "letters to numbers" expected actual
--
-- The 'property' function allows a QuickCheck property to act as an example. (must import @Test.Hspec.HUnit@)
--
-- >   it "can add and remove formatting without changing the number" $ property $
-- >     forAll phoneNumber $ \ n -> unformatPhoneNumber (formatPhoneNumber n) == n
-- >
-- > phoneNumber :: Gen String
-- > phoneNumber = do
-- >   nums <- elements [7,10,11,12,13,14,15]
-- >   vectorOf nums (elements "0123456789")
--

module Test.Hspec.Monadic (

  -- * Types
    Spec
  , Result
  , Specs

  -- * Defining a spec
  , describe
  , it
  , pending

  -- * Running a spec
  , hspec
  , hspecB
  , hspecX
  , hHspec

  -- * Interface to the non-monadic API
  , runSpecM
  , fromSpecList

  -- * Deprecated functions
  , descriptions
) where

import System.IO
import Test.Hspec.Core hiding (describe,descriptions,it)
import qualified Test.Hspec.Core as Core
import qualified Test.Hspec.Runner as Runner

import Control.Monad.Trans.Writer (Writer, execWriter, tell)

type Specs = SpecM ()

newtype SpecM a = SpecM (Writer [UnevaluatedSpec] a)
  deriving Monad

-- | Create a document of the given specs and write it to stdout.
hspec :: Specs -> IO [EvaluatedSpec]
hspec = Runner.hspec . runSpecM

-- | Use in place of @hspec@ to also exit the program with an @ExitCode@
hspecX :: Specs -> IO a
hspecX = Runner.hspecX . runSpecM

-- | Use in place of hspec to also give a @Bool@ success indication
hspecB :: Specs -> IO Bool
hspecB = Runner.hspecB . runSpecM

-- | Create a document of the given specs and write it to the given handle.
--
-- > writeReport filename specs = withFile filename WriteMode (\ h -> hHspec h specs)
--
hHspec :: Handle -> Specs -> IO [EvaluatedSpec]
hHspec h = Runner.hHspec h . runSpecM

-- | Convert a monadic spec into a non-monadic spec.
runSpecM :: Specs -> [UnevaluatedSpec]
runSpecM (SpecM specs) = execWriter specs

-- | Convert a non-monadic spec into a monadic spec.
fromSpecList :: [UnevaluatedSpec] -> Specs
fromSpecList = SpecM . tell

describe :: String -> Specs -> Specs
describe label action = SpecM . tell $ [Core.describe label (runSpecM action)]

it :: Example v => String -> v -> Specs
it label action = (SpecM . tell) [Core.it label action]

-- | DEPRECATED: Use `sequence_` instead.
descriptions :: [Specs] -> Specs
descriptions = sequence_
{-# DEPRECATED descriptions "sequence_ instead" #-}
