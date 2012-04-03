
-- | Hspec is a Behaviour-Driven Development tool for Haskell programmers. BDD is an approach
-- to software development that combines Test-Driven Development, Domain Driven Design, and
-- Acceptance Test-Driven Planning. Hspec helps you do the TDD part of that equation, focusing
-- on the documentation and design aspects of TDD.
--
-- Hspec (and the preceding intro) are based on the Ruby library RSpec. Much of what applies to
-- RSpec also applies to Hspec. Hspec ties together /descriptions/ of behavior and /examples/ of
-- that behavior. The examples can also be run as tests and the output summarises what needs to
-- be implemented.
--
-- The three functions you'll use the most are 'hspec', 'describe', and 'it'. Here is an
-- example of functions that format and unformat phone numbers and the specs for them.
--
-- > import Test.Hspec
-- > import Test.Hspec.QuickCheck
-- > import Test.Hspec.HUnit
-- > import Test.QuickCheck
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
-- > mySpecs = [describe "unformatPhoneNumber" [
--
-- A boolean expression can act as a behavior's example.
--
-- >   it "removes dashes, spaces, and parenthesies"
-- >       (unformatPhoneNumber "(555) 555-1234" == "5555551234"),
--
-- The 'pending' function marks a behavior as pending an example. The example doesn't count as failing.
--
-- >   it "handles non-US phone numbers"
-- >       (pending "need to look up how other cultures format phone numbers"),
--
-- An HUnit 'Test' can act as a behavior's example. (must import @Test.Hspec.HUnit@)
--
-- >   it "removes the \"ext\" prefix of the extension"
-- >       (TestCase $ let expected = "5555551234135"
-- >                       actual   = unformatPhoneNumber "(555) 555-1234 ext 135"
-- >                   in assertEqual "remove extension" expected actual),
--
-- An @IO()@ action is treated like an HUnit 'TestCase'. (must import @Test.Hspec.HUnit@)
--
-- >   it "converts letters to numbers"
-- >       (do
-- >         let expected = "6862377"
-- >         let actual   = unformatPhoneNumber "NUMBERS"
-- >         assertEqual "letters to numbers" expected actual),
--
-- The 'property' function allows a QuickCheck property to act as an example. (must import @Test.Hspec.QuickCheck@)
--
-- >   it "can add and remove formatting without changing the number"
-- >       (property $ forAll phoneNumber $
-- >         \ n -> unformatPhoneNumber (formatPhoneNumber n) == n)
-- >   ]]
-- >
-- > phoneNumber :: Gen String
-- > phoneNumber = do
-- >   nums <- elements [7,10,11,12,13,14,15]
-- >   vectorOf nums (elements "0123456789")
--
module Test.Hspec (

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

  -- * Deprecated functions
  , descriptions
) where

import Test.Hspec.Core
import Test.Hspec.Runner

