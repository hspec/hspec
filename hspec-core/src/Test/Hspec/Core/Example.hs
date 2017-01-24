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
, FailureReason (..)
, safeEvaluateExample
) where

import           Data.Maybe (fromMaybe)
import           Data.List (isPrefixOf)
import qualified Test.HUnit.Lang as HUnit

#if MIN_VERSION_HUnit(1,4,0)
import           Data.CallStack
#endif

import qualified Control.Exception as E
import           Control.DeepSeq
import           Data.Typeable (Typeable)
import qualified Test.QuickCheck as QC
import           Test.Hspec.Expectations (Expectation)

import qualified Test.QuickCheck.State as QC (numSuccessTests, maxSuccessTests)
import qualified Test.QuickCheck.Property as QCP

import           Test.Hspec.Core.QuickCheckUtil
import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Compat

-- | A type class for examples
class Example e where
  type Arg e
  type Arg e = ()
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
data Result = Success (Maybe String) | Pending (Maybe String) | Failure (Maybe Location) FailureReason
  deriving (Eq, Show, Read, Typeable)

data FailureReason = NoReason | Reason String | ExpectedButGot (Maybe String) String String
    deriving (Eq, Show, Read, Typeable)

instance NFData FailureReason where
  rnf reason = case reason of
    NoReason -> ()
    Reason r -> r `deepseq` ()
    ExpectedButGot p e a  -> p `deepseq` e `deepseq` a `deepseq` ()

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

safeEvaluateExample :: Example e => e -> Params -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO (Either E.SomeException Result)
safeEvaluateExample example params around progress = do
  r <- safeTry $ forceResult <$> evaluateExample example params around progress
  return $ case r of
    Left e | Just result <- E.fromException e -> Right result
    Left e | Just hunit <- E.fromException e -> Right (hunitFailureToResult hunit)
    _ -> r
  where
    forceResult :: Result -> Result
    forceResult r = case r of
      Success s -> s `deepseq` r
      Pending m -> m `deepseq` r
      Failure _ m -> m `deepseq` r

instance Example Result where
  type Arg Result = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> Result) where
  type Arg (a -> Result) = a
  evaluateExample example _params action _callback = do
    ref <- newIORef (Success Nothing)
    action (writeIORef ref . example)
    readIORef ref

instance Example Bool where
  type Arg Bool = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> Bool) where
  type Arg (a -> Bool) = a
  evaluateExample p _params action _callback = do
    ref <- newIORef (Success Nothing)
    action $ \a -> example a >>= writeIORef ref
    readIORef ref
    where
      example a
        | p a = return (Success Nothing)
        | otherwise = return (Failure Nothing NoReason)

instance Example Expectation where
  type Arg Expectation = ()
  evaluateExample e = evaluateExample (\() -> e)

hunitFailureToResult :: HUnit.HUnitFailure -> Result
hunitFailureToResult e = case e of
#if MIN_VERSION_HUnit(1,3,0)
  HUnit.HUnitFailure mLoc err ->
#if MIN_VERSION_HUnit(1,5,0)
      case err of
        HUnit.Reason reason -> Failure location (Reason reason)
        HUnit.ExpectedButGot preface expected actual -> Failure location (ExpectedButGot preface expected actual)
#else
      Failure location (Reason err)
#endif
    where
      location = case mLoc of
        Nothing -> Nothing
#if MIN_VERSION_HUnit(1,4,0)
        Just loc -> Just $ Location (srcLocFile loc) (srcLocStartLine loc) (srcLocStartCol loc) ExactLocation
#else
        Just loc -> Just $ Location (HUnit.locationFile loc) (HUnit.locationLine loc) (HUnit.locationColumn loc) ExactLocation
#endif
#else
  HUnit.HUnitFailure err -> Failure Nothing err
#endif

instance Example (a -> Expectation) where
  type Arg (a -> Expectation) = a
  evaluateExample e _ action _ = action e >> return (Success Nothing)

instance Example QC.Property where
  type Arg QC.Property = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> QC.Property) where
  type Arg (a -> QC.Property) = a
  evaluateExample p c action progressCallback = do
    r <- QC.quickCheckWithResult (paramsQuickCheckArgs c) {QC.chatty = False} (QCP.callback qcProgressCallback $ aroundProperty action p)
    return $
      case r of
        QC.Success {QC.labels = m}  -> Success $ addQCLabels m
        QC.Failure {QC.output = m}  -> fromMaybe (Failure Nothing . Reason $ sanitizeFailureMessage r) (parsePending m)
        QC.GaveUp {QC.numTests = n} -> Failure Nothing (Reason $ "Gave up after " ++ pluralize n "test" )
        QC.NoExpectedFailure {}     -> Failure Nothing (Reason $ "No expected failure")
#if MIN_VERSION_QuickCheck(2,8,0)
        QC.InsufficientCoverage {}  -> Failure Nothing (Reason $ "Insufficient coverage")
#endif
    where
      qcProgressCallback = QCP.PostTest QCP.NotCounterexample $
        \st _ -> progressCallback (QC.numSuccessTests st, QC.maxSuccessTests st)

      addQCLabels :: [(String, Int)] -> Maybe String
      addQCLabels [] = Nothing
      addQCLabels ls = Just . unlines $ map (\(label, noOfCases) -> label ++ ": " ++ show noOfCases) ls

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
