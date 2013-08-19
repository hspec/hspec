-- |
-- Stability: stable
--
-- Hspec is a framework for /Behavior-Driven Development (BDD)/ in Haskell. BDD
-- is an approach to software development that combines Test-Driven
-- Development, Domain-Driven Design, and Acceptance Test-Driven Planning.
-- Hspec helps you do the TDD part of that equation, focusing on the
-- documentation and design aspects of TDD.
--
-- Hspec (and the preceding intro) are based on the Ruby library RSpec. Much of
-- what applies to RSpec also applies to Hspec. Hspec ties together
-- /textual descriptions/ of behavior and /examples/ for that behavior.  The
-- examples serve as test cases for the specified behavior.  Hspec's mechanism
-- for examples is extensible.  Support for QuickCheck properties and HUnit
-- tests is included in the core package.
module Test.Hspec (

-- * Introduction
-- $intro

-- * Types
  Spec
, Example

-- * Setting expectations
, module Test.Hspec.Expectations

-- * Defining a spec
, describe
, context
, it
, example
, pending
, pendingWith
, before
, parallel

-- * Running a spec
, hspec
) where

import           Test.Hspec.Core.Type hiding (describe, it)
import           Test.Hspec.Runner
import           Test.Hspec.HUnit ()
import           Test.Hspec.Expectations
import           Test.Hspec.Core (mapSpecItem)
import qualified Test.Hspec.Core as Core

-- $intro
--
-- The three functions you'll use the most are 'hspec', 'describe', and 'it'.
-- Here is an example of functions that format and unformat phone numbers and
-- the specs for them.
--
-- > import Test.Hspec
-- > import Test.QuickCheck
-- > import Test.HUnit
-- >
-- > main :: IO ()
-- > main = hspec spec
--
-- Since the specs are often used to tell you what to implement, it's best to
-- start with undefined functions. Once we have some specs, then you can
-- implement each behavior one at a time, ensuring that each behavior is met
-- and there is no undocumented behavior.
--
-- > unformatPhoneNumber :: String -> String
-- > unformatPhoneNumber = undefined
-- >
-- > formatPhoneNumber :: String -> String
-- > formatPhoneNumber = undefined
--
-- The 'describe' function takes a list of behaviors and examples bound
-- together with the 'it' function
--
-- > spec :: Spec
-- > spec = do
-- >   describe "unformatPhoneNumber" $ do
--
-- A `Bool` can be used as an example.
--
-- >     it "removes dashes, spaces, and parenthesies" $
-- >       unformatPhoneNumber "(555) 555-1234" == "5555551234"
--
--
-- The 'pending' function marks a behavior as pending an example. The example
-- doesn't count as failing.
--
-- >     it "handles non-US phone numbers" $
-- >       pending "need to look up how other cultures format phone numbers"
--
-- An HUnit 'Test.HUnit.Lang.Assertion' can be used as an example.
--
-- >     it "converts letters to numbers" $ do
-- >       let expected = "6862377"
-- >           actual   = unformatPhoneNumber "NUMBERS"
-- >       actual @?= expected
--
--
-- A QuickCheck 'Test.QuickCheck.Property' can be used as an example.
--
-- >     it "can add and remove formatting without changing the number" $ property $
-- >       forAll phoneNumber $ \n -> unformatPhoneNumber (formatPhoneNumber n) == n
-- >
-- > phoneNumber :: Gen String
-- > phoneNumber = do
-- >   n <- elements [7,10,11,12,13,14,15]
-- >   vectorOf n (elements "0123456789")


-- | Combine a list of specs into a larger spec.
describe :: String -> Spec -> Spec
describe label action = fromSpecList [Core.describe label (runSpecM action)]

-- | An alias for `describe`.
context :: String -> Spec -> Spec
context = describe

-- | Create a spec item.
--
-- A spec item consists of:
--
-- * a textual description of a desired behavior
--
-- * an example for that behavior
--
-- > describe "absolute" $ do
-- >   it "returns a positive number when given a negative number" $
-- >     absolute (-1) == 1
it :: Example a => String -> a -> Spec
it label action = fromSpecList [Core.it label action]

-- | This is a type restricted version of `id`.  It can be used to get better
-- error messages on type mismatches.
--
-- Compare e.g.
--
-- > it "exposes some behavior" $ example $ do
-- >   putStrLn
--
-- with
--
-- > it "exposes some behavior" $ do
-- >   putStrLn
example :: Expectation -> Expectation
example = id

-- | Run examples of given spec in parallel.
parallel :: Spec -> Spec
parallel = mapSpecItem $ \item -> item {itemIsParallelizable = True}

-- | Run a custom action before every spec item.
before :: IO () -> Spec -> Spec
before action = mapSpecItem $ \item -> item {itemExample = \params -> (action >> itemExample item params)}
