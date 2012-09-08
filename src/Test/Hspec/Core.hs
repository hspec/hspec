{-# OPTIONS_HADDOCK prune #-}
-- |
-- NOTE: There is a monadic and a non-monadic API.  This is the documentation
-- for the non-monadic API.  The monadic API is more stable, so you may prefer
-- it over this one.  For documentation on the monadic API look at
-- "Test.Hspec".
module Test.Hspec.Core (


-- * Introduction
-- $intro

-- * Types
  Spec
, Specs
, Example (..)
, Pending

-- * Defining a spec
, describe
, it
, pending

-- * Running a spec
, hspec
, hspecB
, hHspec
, Summary (..)

-- * Internals
, quantify
, Result (..)

-- deprecated stuff
, descriptions
, hspecX
, AnyExample
, safeEvaluateExample
, UnevaluatedSpec
) where

import           Test.Hspec.Internal hiding (safeEvaluateExample)
import qualified Test.Hspec.Internal as Internal
import           Test.Hspec.Pending
import           Test.Hspec.Runner
import           Test.Hspec.Util

-- $intro
--
-- The three functions you'll use the most are 'hspec', 'describe', and 'it'.
-- Here is an example of functions that format and unformat phone numbers and
-- the specs for them.
--
-- > import Test.Hspec
-- > import Test.Hspec.QuickCheck
-- > import Test.Hspec.HUnit ()
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
-- > unformatPhoneNumber number = undefined
-- >
-- > formatPhoneNumber :: String -> String
-- > formatPhoneNumber number = undefined
--
-- The 'describe' function takes a list of behaviors and examples bound
-- together with the 'it' function
--
-- > spec = [describe "unformatPhoneNumber" [
--
-- A boolean expression can act as a behavior's example.
--
-- >   it "removes dashes, spaces, and parenthesies" $
-- >     unformatPhoneNumber "(555) 555-1234" == "5555551234"
-- >   ,
--
-- The 'pending' function marks a behavior as pending an example. The example
-- doesn't count as failing.
--
-- >   it "handles non-US phone numbers" $
-- >     pending "need to look up how other cultures format phone numbers"
-- >   ,
--
-- An HUnit 'Test.HUnit.Test' can act as a behavior's example. (must import
-- "Test.Hspec.HUnit")
--
-- >   it "removes the \"ext\" prefix of the extension" $ TestCase $ do
-- >     let expected = "5555551234135"
-- >         actual   = unformatPhoneNumber "(555) 555-1234 ext 135"
-- >     expected @?= actual
-- >   ,
--
-- An @IO()@ action is treated like an HUnit 'TestCase'. (must import
-- "Test.Hspec.HUnit")
--
-- >   it "converts letters to numbers" $ do
-- >     let expected = "6862377"
-- >         actual   = unformatPhoneNumber "NUMBERS"
-- >     actual @?= expected
-- >   ,
--
-- The 'property' function allows a QuickCheck property to act as an example.
-- (must import "Test.Hspec.QuickCheck")
--
-- >   it "can add and remove formatting without changing the number" $ property $
-- >     forAll phoneNumber $ \n -> unformatPhoneNumber (formatPhoneNumber n) == n
-- >   ]]
-- >
-- > phoneNumber :: Gen String
-- > phoneNumber = do
-- >   n <- elements [7,10,11,12,13,14,15]
-- >   vectorOf n (elements "0123456789")

{-# DEPRECATED UnevaluatedSpec "use Spec instead" #-}
type UnevaluatedSpec = Spec

{-# DEPRECATED descriptions "this is no longer needed, and will be removed in a future release" #-}
descriptions :: Specs -> Specs
descriptions = id

{-# DEPRECATED AnyExample "This will be removed with the next major release.  If you still need this, raise your voice!" #-}
type AnyExample  = IO Result

{-# DEPRECATED safeEvaluateExample "This will be removed with the next major release.  If you still need this, raise your voice!" #-}
safeEvaluateExample :: AnyExample -> IO Result
safeEvaluateExample = Internal.safeEvaluateExample
