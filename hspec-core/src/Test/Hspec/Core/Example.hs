{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Test.Hspec.Core.Example (
  Example (..)
, Params (..)
, defaultParams
, ActionWith
, Progress
, ProgressCallback
, Result (..)
, Location (..)
, FailureReason (..)
, safeEvaluateExample
) where

import           Data.List (isPrefixOf, stripPrefix)
import qualified Test.HUnit.Lang as HUnit

#if MIN_VERSION_HUnit(1,4,0)
import           Data.CallStack
#endif

import           Control.Exception
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
data Result =
    Success String
  | Pending (Maybe Location) (Maybe String)
  | Failure (Maybe Location) FailureReason
  deriving (Eq, Show, Read, Typeable)

data FailureReason =
    NoReason
  | Reason String
  | ExpectedButGot (Maybe String) String String
  deriving (Eq, Show, Read, Typeable)

instance NFData FailureReason where
  rnf reason = case reason of
    NoReason -> ()
    Reason r -> r `deepseq` ()
    ExpectedButGot p e a  -> p `deepseq` e `deepseq` a `deepseq` ()

instance Exception Result

-- | @Location@ is used to represent source locations.
data Location = Location {
  locationFile :: FilePath
, locationLine :: Int
, locationColumn :: Int
} deriving (Eq, Show, Read)

safeEvaluateExample :: Example e => e -> Params -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO (Either SomeException Result)
safeEvaluateExample example params around progress = do
  r <- safeTry $ forceResult <$> evaluateExample example params around progress
  return $ case r of
    Left e | Just result <- fromException e -> Right result
    Left e | Just hunit <- fromException e -> Right (hunitFailureToResult hunit)
    _ -> r
  where
    forceResult :: Result -> Result
    forceResult r = case r of
      Success s -> s `deepseq` r
      Pending _ m -> m `deepseq` r
      Failure _ m -> m `deepseq` r

instance Example Result where
  type Arg Result = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> Result) where
  type Arg (a -> Result) = a
  evaluateExample example _params action _callback = do
    ref <- newIORef (Success "")
    action (writeIORef ref . example)
    readIORef ref

instance Example Bool where
  type Arg Bool = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> Bool) where
  type Arg (a -> Bool) = a
  evaluateExample p _params action _callback = do
    ref <- newIORef (Success "")
    action $ \a -> example a >>= writeIORef ref
    readIORef ref
    where
      example a
        | p a = return (Success "")
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
        Just loc -> Just $ Location (srcLocFile loc) (srcLocStartLine loc) (srcLocStartCol loc)
#else
        Just loc -> Just $ Location (HUnit.locationFile loc) (HUnit.locationLine loc) (HUnit.locationColumn loc)
#endif
#else
  HUnit.HUnitFailure err -> Failure Nothing (Reason err)
#endif

instance Example (a -> Expectation) where
  type Arg (a -> Expectation) = a
  evaluateExample e _ action _ = action e >> return (Success "")

instance Example QC.Property where
  type Arg QC.Property = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> QC.Property) where
  type Arg (a -> QC.Property) = a
  evaluateExample p c action progressCallback = do
    r <- QC.quickCheckWithResult (paramsQuickCheckArgs c) {QC.chatty = False} (QCP.callback qcProgressCallback $ aroundProperty action p)
    return $ fromQuickCheckResult r
    where
      qcProgressCallback = QCP.PostTest QCP.NotCounterexample $
        \st _ -> progressCallback (QC.numSuccessTests st, QC.maxSuccessTests st)


fromQuickCheckResult :: QC.Result -> Result
fromQuickCheckResult r = case r of
  QC.Success {QC.labels = m}  -> Success (formatLabels $
#if MIN_VERSION_QuickCheck(2,10,0)
    map (fmap round)
#endif
    m
    )
  QC.Failure {} -> case QC.theException r of
    Just e | Just result <- fromException e -> result
    _ -> Failure Nothing . Reason $ sanitizeFailureMessage r
  QC.GaveUp {QC.numTests = n} -> Failure Nothing (Reason $ "Gave up after " ++ pluralize n "test" )
  QC.NoExpectedFailure {} -> Failure Nothing (Reason $ "No expected failure")
#if MIN_VERSION_QuickCheck(2,8,0)
  QC.InsufficientCoverage {} -> Failure Nothing (Reason $ "Insufficient coverage")
#endif
  where
    formatLabels :: [(String, Int)] -> String
    formatLabels = unlines . map (\ (label, n) -> showPercent n ++ label)
      where
        showPercent n = show n ++ "% " ++ if n < 10 then " " else ""

    sanitizeFailureMessage :: QC.Result -> String
    sanitizeFailureMessage result = let m = QC.output result in strip $
      case QC.theException result of
        Just e -> let numbers = formatNumbers result in
          "uncaught exception: " ++ formatException e ++ "\n" ++ numbers ++ "\n" ++ case lines m of
            x:xs | x == (exceptionPrefix ++ show e ++ "' " ++ numbers ++ ": ") -> unlines xs
            _ -> m
        Nothing ->
          (addFalsifiable . stripFailed) m

    addFalsifiable :: String -> String
    addFalsifiable m
      | "(after " `isPrefixOf` m = "Falsifiable " ++ m
      | otherwise = m

    stripFailed :: String -> String
    stripFailed m = case stripPrefix "*** Failed! " m of
      Just xs -> xs
      Nothing -> m

    exceptionPrefix = "*** Failed! Exception: '"
