{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Test.Hspec.Internal (
  Spec
, SpecM (..)
, runSpecM
, SpecTree (..)
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

import           Control.Monad.Trans.Writer (Writer, execWriter)

type Spec = SpecM ()

-- | A writer monad for `SpecTree` forests.
newtype SpecM a = SpecM (Writer [SpecTree] a)
  deriving Monad

-- | Convert a `Spec` to a forest of `SpecTree`s.
runSpecM :: Spec -> [SpecTree]
runSpecM (SpecM specs) = execWriter specs

-- | The result of running an example.
data Result = Success | Pending (Maybe String) | Fail String
  deriving (Eq, Show)

data Params = Params {
  paramsQuickCheckArgs :: QC.Args
}

defaultParams :: Params
defaultParams = Params QC.stdArgs

-- | Internal representation of a spec.
data SpecTree =
    SpecGroup String [SpecTree]
  | SpecExample String (Params -> IO Result)

-- | The @describe@ function combines a list of specs into a larger spec.
describe :: String -> [SpecTree] -> SpecTree
describe = SpecGroup

-- |
-- Create a set of specifications for a specific type being described.  Once
-- you know what you want specs for, use this.
--
-- > describe "abs" [
-- >   it "returns a positive number given a negative number"
-- >     (abs (-1) == 1)
-- >   ]
it :: Example a => String -> a -> SpecTree
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
