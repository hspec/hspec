{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Test.Hspec.Internal (
  Spec (..)
, Specs
, Example (..)
, Result (..)

, Params (..)
, defaultParams

, describe
, it
) where

import qualified Control.Exception as E
import           Test.Hspec.Util
import           Test.Hspec.Expectations
import           Test.HUnit.Lang (HUnitFailure(..))
import qualified Test.QuickCheck as QC

-- | A list of specs.
type Specs = [Spec]

-- | The result of running an example.
data Result = Success | Pending (Maybe String) | Fail String
  deriving (Eq, Show)

data Params = Params {
  paramsQuickCheckArgs :: QC.Args
}

defaultParams :: Params
defaultParams = Params QC.stdArgs

-- | Internal representation of a spec.
data Spec = SpecGroup String [Spec]
          | SpecExample String (Params -> IO Result)

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
  evaluateExample :: Params -> a -> IO Result

instance Example Bool where
  evaluateExample _ b = if b then return Success else return (Fail "")

instance Example Expectation where
  evaluateExample _ action = (action >> return Success) `E.catch` \(HUnitFailure err) -> return (Fail err)

instance Example Result where
  evaluateExample _ r = r `seq` return r

instance Example QC.Property where
  evaluateExample c p = do
    r <- QC.quickCheckWithResult (paramsQuickCheckArgs c) p
    return $
      case r of
        QC.Success {}               -> Success
        f@(QC.Failure {})           -> Fail (QC.output f)
        QC.GaveUp {QC.numTests = n} -> Fail ("Gave up after " ++ quantify n "test" )
        QC.NoExpectedFailure {}     -> Fail ("No expected failure")
