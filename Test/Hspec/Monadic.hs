{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- There is a monadic and a non-monadic API.  This is the documentation for the
-- monadic API.  The monadic API is suitable for most use cases, and it is more
-- stable than the non-monadic API.
--
-- For documentation on the non-monadic API look at "Test.Hspec".
--
module Test.Hspec.Monadic (
-- * Introduction
-- $intro
-- * Types
  Spec
, Example
, Pending

-- * Defining a spec
, describe
-- , context
, it
, pending

-- * Running a spec
, hspec
, hspecB
, hHspec

-- * Interface to the non-monadic API
, runSpecM
, fromSpecList

-- * Deprecated types and functions
, Specs
, descriptions
, hspecX
) where

import           System.IO
import           Test.Hspec.Core (EvaluatedSpec, Example)
import qualified Test.Hspec.Core as Core
import qualified Test.Hspec.Runner as Runner
import           Test.Hspec.Pending (Pending)
import qualified Test.Hspec.Pending as Pending

import           Control.Monad.Trans.Writer (Writer, execWriter, tell)

-- $intro
--
-- The three functions you'll use the most are 'hspecX', 'describe', and 'it'.
-- Here is an example of functions that format and unformat phone numbers and
-- the specs for them.
--
-- > import Test.Hspec.Monadic
-- > import Test.Hspec.QuickCheck
-- > import Test.Hspec.HUnit ()
-- > import Test.QuickCheck
-- > import Test.HUnit
-- >
-- > main = hspecX mySpecs
--
-- Since the specs are often used to tell you what to implement, it's best to
-- start with undefined functions. Once we have some specs, then you can
-- implement each behavior one at a time, ensuring that each behavior is met
-- and there is no undocumented behavior.
--
-- > unformatPhoneNumber :: String -> String
-- > unformatPhoneNumber number = undefined
-- >
-- > formatPhoneNumber :: String -> String
-- > formatPhoneNumber number = undefined
--
-- The 'describe' function takes a list of behaviors and examples bound
-- together with the 'it' function
--
-- > mySpecs = describe "unformatPhoneNumber" $ do
--
-- A boolean expression can act as a behavior's example.
--
-- >   it "removes dashes, spaces, and parenthesies" $
-- >     unformatPhoneNumber "(555) 555-1234" == "5555551234"
--
--
-- The 'pending' function marks a behavior as pending an example. The example
-- doesn't count as failing.
--
-- >   it "handles non-US phone numbers" $
-- >     pending "need to look up how other cultures format phone numbers"
--
--
-- An HUnit 'Test.HUnit.Test' can act as a behavior's example. (must import
-- "Test.Hspec.HUnit")
--
-- >   it "removes the \"ext\" prefix of the extension" $ TestCase $ do
-- >     let expected = "5555551234135"
-- >         actual   = unformatPhoneNumber "(555) 555-1234 ext 135"
-- >     expected @?= actual
--
--
-- An @IO()@ action is treated like an HUnit 'TestCase'. (must import
-- "Test.Hspec.HUnit")
--
-- >   it "converts letters to numbers" $ do
-- >     let expected = "6862377"
-- >         actual   = unformatPhoneNumber "NUMBERS"
-- >     actual @?= expected
--
--
-- The 'property' function allows a QuickCheck property to act as an example.
-- (must import "Test.Hspec.QuickCheck")
--
-- >   it "can add and remove formatting without changing the number" $ property $
-- >     forAll phoneNumber $ \n -> unformatPhoneNumber (formatPhoneNumber n) == n
-- >
-- > phoneNumber :: Gen String
-- > phoneNumber = do
-- >   n <- elements [7,10,11,12,13,14,15]
-- >   vectorOf n (elements "0123456789")
--

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
hHspec :: Handle -> Spec -> IO [EvaluatedSpec]
hHspec h = Runner.hHspec h . runSpecM

-- | Convert a monadic spec into a non-monadic spec.
runSpecM :: Spec -> [Core.Spec]
runSpecM (SpecM specs) = execWriter specs

-- | Convert a non-monadic spec into a monadic spec.
fromSpecList :: [Core.Spec] -> Spec
fromSpecList = SpecM . tell

describe :: String -> Spec -> Spec
describe label action = SpecM . tell $ [Core.describe label (runSpecM action)]

{-
context :: String -> Spec -> Spec
context = describe
-}

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

-- | DEPRECATED: Use `Spec` instead
type Specs = SpecM ()

-- | DEPRECATED: Use `sequence_` instead.
descriptions :: [Spec] -> Spec
descriptions = sequence_
{-# DEPRECATED descriptions "use sequence_ instead" #-}

-- | DEPRECATED: Use `hspec` instead.
hspecX :: Spec -> IO a
hspecX = Runner.hspecX . runSpecM
