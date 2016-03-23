{-# LANGUAGE CPP, TypeFamilies, FlexibleInstances, TypeSynonymInstances, DeriveDataTypeable #-}
module Test.Hspec.Core.Example (
  Example (..)
, Params (..)
, defaultParams
, ActionWith
, Progress
, ProgressCallback
, Result (..)
, Location (..)
, LocationAccuracy (..)
) where

import           Data.Maybe (fromMaybe)
import           Data.List (isPrefixOf)
import qualified Test.HUnit.Lang as HUnit
import qualified Control.Exception as E
import           Data.Typeable (Typeable)
import qualified Test.QuickCheck as QC
import           Test.Hspec.Expectations (Expectation)

import qualified Test.QuickCheck.State as QC
import qualified Test.QuickCheck.Property as QCP

import           Test.Hspec.Core.QuickCheckUtil
import           Test.Hspec.Core.Util
import           Test.Hspec.Compat

-- | A type class for examples
class Example e where
  type Arg e
#if __GLASGOW_HASKELL__ >= 704
  type Arg e = ()
#endif
  evaluateExample :: e -> Params -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO Result

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
data Result = Success | Pending (Maybe String) | Fail (Maybe Location) String
  deriving (Eq, Show, Read, Typeable)

instance E.Exception Result

-- | @Location@ is used to represent source locations.
data Location = Location {
  locationFile :: FilePath
, locationLine :: Int
, locationColumn :: Int
, locationAccuracy :: LocationAccuracy
} deriving (Eq, Show, Read)

-- | A marker for source locations
data LocationAccuracy =
  -- | The source location is accurate
  ExactLocation |
  -- | The source location was determined on a best-effort basis and my be
  -- wrong or inaccurate
  BestEffort
  deriving (Eq, Show, Read)

instance Example Bool where
  type Arg Bool = ()
  evaluateExample b _ _ _ = if b then return Success else return (Fail Nothing "")

instance Example Expectation where
  type Arg Expectation = ()
  evaluateExample e = evaluateExample (\() -> e)

hunitFailureToResult :: HUnit.HUnitFailure -> Result
hunitFailureToResult e = case e of
#if MIN_VERSION_HUnit(1,3,0)
  HUnit.HUnitFailure mLoc err -> Fail location err
    where
      location = case mLoc of
        Nothing -> Nothing
        Just loc -> Just $ Location (HUnit.locationFile loc) (HUnit.locationLine loc) (HUnit.locationColumn loc) ExactLocation
#else
  HUnit.HUnitFailure err -> Fail Nothing err
#endif

instance Example (a -> Expectation) where
  type Arg (a -> Expectation) = a
  evaluateExample e _ action _ = (action e >> return Success) `E.catches` [
      E.Handler (return . hunitFailureToResult)
    , E.Handler (return :: Result -> IO Result)
    ]

instance Example Result where
  type Arg Result = ()
  evaluateExample r _ _ _ = return r

instance Example QC.Property where
  type Arg QC.Property = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> QC.Property) where
  type Arg (a -> QC.Property) = a
  evaluateExample p c action progressCallback = do
    r <- QC.quickCheckWithResult (paramsQuickCheckArgs c) {QC.chatty = False} (QCP.callback qcProgressCallback $ aroundProperty action p)
    return $
      case r of
        QC.Success {}               -> Success
        QC.Failure {QC.output = m}  -> fromMaybe (Fail Nothing $ sanitizeFailureMessage r) (parsePending m)
        QC.GaveUp {QC.numTests = n} -> Fail Nothing ("Gave up after " ++ pluralize n "test" )
        QC.NoExpectedFailure {}     -> Fail Nothing ("No expected failure")
#if MIN_VERSION_QuickCheck(2,8,0)
        QC.InsufficientCoverage {}  -> Fail Nothing ("Insufficient coverage")
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
