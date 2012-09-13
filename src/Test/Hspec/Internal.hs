{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Test.Hspec.Internal (
  Spec (..)
, Specs
, Example (..)
, Result (..)

, describe
, it
) where

import qualified Control.Exception as E
import           System.IO.Silently
import           Test.Hspec.Config (Config)
import           Test.Hspec.Expectations
import           Test.HUnit.Lang (HUnitFailure(..))

-- | A list of specs.
type Specs = [Spec]

-- | The result of running an example.
data Result = Success | Pending (Maybe String) | Fail String
  deriving (Eq, Show)

-- | Internal representation of a spec.
data Spec = SpecGroup String [Spec]
          | SpecExample String (Config -> IO Result)

-- | The @describe@ function combines a list of specs into a larger spec.
describe :: String -> [Spec] -> Spec
describe = SpecGroup

-- |
-- Create a set of specifications for a specific type being described.  Once
-- you know what you want specs for, use this.
--
-- > describe "abs" [
-- >   it "returns a positive number given a negative number"
-- >     (abs (-1) == 1)
-- >   ]
it :: Example a => String -> a -> Spec
it s e = SpecExample s (`evaluateExample` e)

-- | A type class for examples.
class Example a where
  evaluateExample :: Config -> a -> IO Result

instance Example Bool where
  evaluateExample _ b = if b then return Success else return (Fail "")

instance Example Expectation where
  evaluateExample _ action = (silence action >> return Success) `E.catch` \(HUnitFailure err) -> return (Fail err)

instance Example Result where
  evaluateExample _ r = r `seq` return r
