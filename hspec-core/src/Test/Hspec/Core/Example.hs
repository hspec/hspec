{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Hspec.Core.Example (
-- RE-EXPORTED from Test.Hspec.Core.Spec
  Example (..)
, Params (..)
, defaultParams
, ActionWith
, Progress
, ProgressCallback
, Result(..)
, ResultStatus (..)
, Location (..)
, FailureReason (..)
, safeEvaluate
, safeEvaluateExample
-- END RE-EXPORTED from Test.Hspec.Core.Spec
, safeEvaluateResultStatus
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified Test.HUnit.Lang as HUnit

import           Data.CallStack

import           Control.Exception
import           Control.DeepSeq
import           Data.Typeable (Typeable)
import qualified Test.QuickCheck as QC
import           Test.Hspec.Expectations (Expectation)

import qualified Test.QuickCheck.State as QC (numSuccessTests, maxSuccessTests)
import qualified Test.QuickCheck.Property as QCP

import           Test.Hspec.Core.QuickCheckUtil
import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Example.Location

-- | A type class for examples
class Example env e where
  evaluateExample :: e -> Params -> (ActionWith env -> IO ()) -> ProgressCallback -> IO Result

data Params = Params {
  paramsQuickCheckArgs  :: QC.Args
, paramsSmallCheckDepth :: Maybe Int
} deriving (Show)

defaultParams :: Params
defaultParams = Params {
  paramsQuickCheckArgs = QC.stdArgs
, paramsSmallCheckDepth = Nothing
}

type Progress = (Int, Int)
type ProgressCallback = Progress -> IO ()

-- | An `IO` action that expects an argument of type @a@
type ActionWith a = a -> IO ()

-- | The result of running an example
data Result = Result {
  resultInfo :: String
, resultStatus :: ResultStatus
} deriving (Show, Typeable)

data ResultStatus =
    Success
  | Pending (Maybe Location) (Maybe String)
  | Failure (Maybe Location) FailureReason
  deriving (Show, Typeable)

data FailureReason =
    NoReason
  | Reason String
  | ExpectedButGot (Maybe String) String String
  | Error (Maybe String) SomeException
  deriving (Show, Typeable)

instance NFData FailureReason where
  rnf reason = case reason of
    NoReason -> ()
    Reason r -> r `deepseq` ()
    ExpectedButGot p e a  -> p `deepseq` e `deepseq` a `deepseq` ()
    Error m e -> m `deepseq` e `seq` ()

instance Exception ResultStatus

safeEvaluateExample :: Example env e => e -> Params -> (ActionWith env -> IO ()) -> ProgressCallback -> IO Result
safeEvaluateExample example params around progress = safeEvaluate $ forceResult <$> evaluateExample example params around progress
  where
    forceResult :: Result -> Result
    forceResult r@(Result info status) = info `deepseq` (forceResultStatus status) `seq` r

    forceResultStatus :: ResultStatus -> ResultStatus
    forceResultStatus r = case r of
      Success -> r
      Pending _ m -> m `deepseq` r
      Failure _ m -> m `deepseq` r

safeEvaluate :: IO Result -> IO Result
safeEvaluate action = do
  r <- safeTry action
  return $ case r of
    Left e -> Result "" (toResultStatus e)
    Right result -> result

safeEvaluateResultStatus :: IO ResultStatus -> IO ResultStatus
safeEvaluateResultStatus action = either toResultStatus id <$> safeTry action

toResultStatus :: SomeException -> ResultStatus
toResultStatus e
  | Just result <- fromException e = result
  | Just hunit <- fromException e = hunitFailureToResult Nothing hunit
  | otherwise = Failure Nothing $ Error Nothing e

instance Example env Result where
  evaluateExample example _params action _callback = do
    ref <- newIORef (Result "" Success)
    action $ \_ -> do
        r <- evaluate example
        writeIORef ref r
    readIORef ref

instance (env ~ a) => Example env (a -> Result) where
  evaluateExample example _params action _callback = do
    ref <- newIORef (Result "" Success)
    action (evaluate . example >=> writeIORef ref)
    readIORef ref

instance Example env Bool where
  evaluateExample p _params action _callback = do
    ref <- newIORef (Result "" Success)
    action (\_ -> evaluate example >>= writeIORef ref)
    readIORef ref
    where
      example
        | p = Result "" Success
        | otherwise = Result "" $ Failure Nothing NoReason

instance (env ~ a) => Example env (a -> Bool) where
  evaluateExample p _params action _callback = do
    ref <- newIORef (Result "" Success)
    action (evaluate . example >=> writeIORef ref)
    readIORef ref
    where
      example a
        | p a = Result "" Success
        | otherwise = Result "" $ Failure Nothing NoReason

instance Example env Expectation where
  evaluateExample e _ action _ = action (\_ -> e) >> return (Result "" Success)

hunitFailureToResult :: Maybe String -> HUnit.HUnitFailure -> ResultStatus
hunitFailureToResult pre e = case e of
  HUnit.HUnitFailure mLoc err ->
      case err of
        HUnit.Reason reason -> Failure location (Reason $ addPre reason)
        HUnit.ExpectedButGot preface expected actual -> Failure location (ExpectedButGot (addPreMaybe preface) expected actual)
          where
            addPreMaybe :: Maybe String -> Maybe String
            addPreMaybe xs = case (pre, xs) of
              (Just x, Just y) -> Just (x ++ "\n" ++ y)
              _ -> pre <|> xs
    where
      location = case mLoc of
        Nothing -> Nothing
        Just loc -> Just $ Location (srcLocFile loc) (srcLocStartLine loc) (srcLocStartCol loc)
  where
    addPre :: String -> String
    addPre xs = case pre of
      Just x -> x ++ "\n" ++ xs
      Nothing -> xs

instance (env ~ a) => Example env (a -> Expectation) where
  evaluateExample e _ action _ = action e >> return (Result "" Success)

instance Example a QC.Property where
  evaluateExample p c action progressCallback = do
    r <- QC.quickCheckWithResult (paramsQuickCheckArgs c) {QC.chatty = False} (QCP.callback qcProgressCallback $ aroundProperty action (\_ -> p))
    return $ fromQuickCheckResult r
    where
      qcProgressCallback = QCP.PostTest QCP.NotCounterexample $
        \st _ -> progressCallback (QC.numSuccessTests st, QC.maxSuccessTests st)

instance (env ~ a) => Example env (a -> QC.Property) where
  evaluateExample p c action progressCallback = do
    r <- QC.quickCheckWithResult (paramsQuickCheckArgs c) {QC.chatty = False} (QCP.callback qcProgressCallback $ aroundProperty action p)
    return $ fromQuickCheckResult r
    where
      qcProgressCallback = QCP.PostTest QCP.NotCounterexample $
        \st _ -> progressCallback (QC.numSuccessTests st, QC.maxSuccessTests st)

fromQuickCheckResult :: QC.Result -> Result
fromQuickCheckResult r = case parseQuickCheckResult r of
  QuickCheckResult _ info (QuickCheckOtherFailure err) -> Result info $ Failure Nothing (Reason err)
  QuickCheckResult _ info QuickCheckSuccess -> Result info Success
  QuickCheckResult n info (QuickCheckFailure QCFailure{..}) -> case quickCheckFailureException of
    Just e | Just result <- fromException e -> Result info result
    Just e | Just hunit <- fromException e -> Result info $ hunitFailureToResult (Just hunitAssertion) hunit
    Just e -> failure (uncaughtException e)
    Nothing -> failure falsifiable
    where
      failure = Result info . Failure Nothing . Reason

      numbers = formatNumbers n quickCheckFailureNumShrinks

      hunitAssertion :: String
      hunitAssertion = intercalate "\n" [
          "Falsifiable " ++ numbers ++ ":"
        , indent (unlines quickCheckFailureCounterexample)
        ]

      uncaughtException e = intercalate "\n" [
          "uncaught exception: " ++ formatException e
        , numbers
        , indent (unlines quickCheckFailureCounterexample)
        ]

      falsifiable = intercalate "\n" [
          quickCheckFailureReason ++ " " ++ numbers ++ ":"
        , indent (unlines quickCheckFailureCounterexample)
        ]

indent :: String -> String
indent = intercalate "\n" . map ("  " ++) . lines
