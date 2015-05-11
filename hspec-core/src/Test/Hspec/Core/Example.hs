{-# LANGUAGE CPP, TypeFamilies, FlexibleInstances, TypeSynonymInstances, DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
-- TODO: Have a separate type class that captures the idea of what an Example needs to support for arbitrary arguments.
module Test.Hspec.Core.Example (
  Example (..)
, Params (..)
, defaultParams
, ActionWith
, Progress
, ProgressCallback
, Result (..)
) where

import           Data.Maybe (fromMaybe)
import           Data.List (isPrefixOf)
import           Test.HUnit.Lang (HUnitFailure(..))
import qualified Control.Exception as E
import           Data.Typeable (Typeable)
import qualified Test.QuickCheck as QC
import           Test.Hspec.Expectations (Expectation)

import qualified Test.QuickCheck.State as QC
import qualified Test.QuickCheck.Property as QCP

import           Test.Hspec.Core.QuickCheckUtil
import           Test.Hspec.Core.Uncurry
import           Test.Hspec.Core.Util
import           Test.Hspec.Compat

-- | A type class for examples
class Example e where
  type Arg e
  type T e
  evaluateExample :: e -> Params -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO Result
  toSimple :: e -> (Arg e -> T e)

data Params = Params {
  paramsQuickCheckArgs  :: QC.Args
, paramsSmallCheckDepth :: Int
} deriving (Show)

defaultParams :: Params
defaultParams = Params {
  paramsQuickCheckArgs = QC.stdArgs
, paramsSmallCheckDepth = 5
}

type Progress = (Int, Int)
type ProgressCallback = Progress -> IO ()

-- | An `IO` action that expects an argument of type @a@
type ActionWith a = a -> IO ()

-- | The result of running an example
data Result = Success | Pending (Maybe String) | Fail String
  deriving (Eq, Show, Read, Typeable)

instance E.Exception Result

class Testable t where
  evaluateTestable :: (a -> t) -> Params -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result

instance Example Bool where
  type Arg Bool = ()
  type T Bool = Bool
  evaluateExample b _ _ _ = if b then return Success else return (Fail "")
  toSimple b = \ () -> b

instance Example Expectation where
  type Arg Expectation = ()
  type T Expectation = Expectation
  evaluateExample e params around progress =
    evaluateTestable (\ () -> e) params around progress
  toSimple e = \ () -> e

instance Testable Expectation where
  evaluateTestable = evaluateExpectation

evaluateExpectation :: (a -> Expectation) -> Params -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
evaluateExpectation e _ around _ = ((around e) >> return Success) `E.catches` [
    E.Handler (\(HUnitFailure err) -> return (Fail err))
  , E.Handler (return :: Result -> IO Result)
  ]

instance (Example e, Testable (T e)) => Example (a -> e) where
  type Arg (a -> e) = (a, Arg e)
  type T (a -> e) = T e
  evaluateExample e params around progress =
    evaluateTestable (toSimple e) params around progress
  toSimple f = \ (a, innerArg) -> toSimple (f a) innerArg

instance Example Result where
  type Arg Result = ()
  type T Result = Result
  evaluateExample r _ _ _ = return r
  toSimple r = \ () -> r

instance Example QC.Property where
  type Arg QC.Property = ()
  type T QC.Property = QC.Property
  evaluateExample = evaluateProperty . uncurryT
  toSimple p = \ () -> p

instance Testable QC.Property where
  evaluateTestable = evaluateProperty

evaluateProperty :: (a -> QC.Property) -> Params -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
evaluateProperty p params around progressCallback = do
    r <- QC.quickCheckWithResult (paramsQuickCheckArgs params) {QC.chatty = False} (QCP.callback qcProgressCallback $ aroundProperty around p)
    return $
      case r of
        QC.Success {}               -> Success
        QC.Failure {QC.output = m}  -> fromMaybe (Fail $ sanitizeFailureMessage r) (parsePending m)
        QC.GaveUp {QC.numTests = n} -> Fail ("Gave up after " ++ pluralize n "test" )
        QC.NoExpectedFailure {}     -> Fail ("No expected failure")
#if MIN_VERSION_QuickCheck(2,8,0)
        QC.InsufficientCoverage {}  -> Fail ("Insufficient coverage")
#endif
    where
      qcProgressCallback = QCP.PostTest QCP.NotCounterexample $
        \st _ -> progressCallback (QC.numSuccessTests st, QC.maxSuccessTests st)

      sanitizeFailureMessage :: QC.Result -> String
      sanitizeFailureMessage r = let m = QC.output r in strip $
#if MIN_VERSION_QuickCheck(2,7,0)
        case QC.theException r of
          Just e -> let numbers = formatNumbers r in
            "uncaught exception: " ++ formatException e ++ " " ++ numbers ++ "\n" ++ case lines m of
              x:xs | x == (exceptionPrefix ++ show e ++ "' " ++ numbers ++ ": ") -> unlines xs
              _ -> m
          Nothing ->
#endif
            (addFalsifiable . stripFailed) m

      addFalsifiable :: String -> String
      addFalsifiable m
        | "(after " `isPrefixOf` m = "Falsifiable " ++ m
        | otherwise = m

      stripFailed :: String -> String
      stripFailed m
        | prefix `isPrefixOf` m = drop n m
        | otherwise = m
        where
          prefix = "*** Failed! "
          n = length prefix

      parsePending :: String -> Maybe Result
      parsePending m
        | exceptionPrefix `isPrefixOf` m = (readMaybe . takeWhile (/= '\'') . drop n) m
        | otherwise = Nothing
        where
          n = length exceptionPrefix

      exceptionPrefix = "*** Failed! Exception: '"
