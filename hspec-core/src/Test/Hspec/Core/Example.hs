{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
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
, exceptionToResultStatus
, toLocation
, hunitFailureToResult
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified Test.HUnit.Lang as HUnit

import           Data.CallStack (SrcLoc(..))

import           Control.DeepSeq
import qualified Test.QuickCheck as QC
import           Test.Hspec.Expectations (Expectation)

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.QuickCheck.Util (liftHook)
import           Test.Hspec.Core.Example.Location

-- | A type class for examples
class Example e where
  type Arg e
  type Arg e = ()
  evaluateExample :: e -> Params -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO Result

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
} deriving Show

data ResultStatus =
    Success
  | Pending (Maybe Location) (Maybe String)
  | Failure (Maybe Location) FailureReason
  deriving Show

data FailureReason =
    NoReason
  | Reason String
  | ColorizedReason String
  | ExpectedButGot (Maybe String) String String
  | Error (Maybe String) SomeException
  deriving Show

instance NFData FailureReason where
  rnf reason = case reason of
    NoReason -> ()
    Reason r -> r `deepseq` ()
    ColorizedReason r -> r `deepseq` ()
    ExpectedButGot p e a  -> p `deepseq` e `deepseq` a `deepseq` ()
    Error m e -> m `deepseq` show e `deepseq` ()

instance Exception ResultStatus

forceResult :: Result -> Result
forceResult r@(Result info status) = info `deepseq` (forceResultStatus status) `seq` r

forceResultStatus :: ResultStatus -> ResultStatus
forceResultStatus r = case r of
  Success -> r
  Pending _ m -> m `deepseq` r
  Failure _ m -> m `deepseq` r

safeEvaluateExample :: Example e => e -> Params -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO Result
safeEvaluateExample example params around = safeEvaluate . evaluateExample example params around

safeEvaluate :: IO Result -> IO Result
safeEvaluate action = do
  r <- safeTry $ forceResult <$> action
  case r of
    Left e -> Result "" <$> exceptionToResultStatus e
    Right result -> return result

safeEvaluateResultStatus :: IO ResultStatus -> IO ResultStatus
safeEvaluateResultStatus action = do
  r <- safeTry $ forceResultStatus <$> action
  case r of
    Left e -> exceptionToResultStatus e
    Right status -> return status

exceptionToResultStatus :: SomeException -> IO ResultStatus
exceptionToResultStatus = safeEvaluateResultStatus . pure . toResultStatus
  where
    toResultStatus :: SomeException -> ResultStatus
    toResultStatus e
      | Just result <- fromException e = result
      | Just hunit <- fromException e = hunitFailureToResult Nothing hunit
      | otherwise = Failure Nothing $ Error Nothing e

instance Example Result where
  type Arg Result = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> Result) where
  type Arg (a -> Result) = a
  evaluateExample example _params hook _callback = do
    liftHook (Result "" Success) hook (evaluate . example)

instance Example Bool where
  type Arg Bool = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> Bool) where
  type Arg (a -> Bool) = a
  evaluateExample p _params hook _callback = do
    liftHook (Result "" Success) hook (evaluate . example)
    where
      example a
        | p a = Result "" Success
        | otherwise = Result "" $ Failure Nothing NoReason

instance Example Expectation where
  type Arg Expectation = ()
  evaluateExample e = evaluateExample (\() -> e)

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
        Just loc -> Just $ toLocation loc
  where
    addPre :: String -> String
    addPre xs = case pre of
      Just x -> x ++ "\n" ++ xs
      Nothing -> xs

toLocation :: SrcLoc -> Location
toLocation loc = Location (srcLocFile loc) (srcLocStartLine loc) (srcLocStartCol loc)

instance Example (a -> Expectation) where
  type Arg (a -> Expectation) = a
  evaluateExample e _params hook _ = hook e >> return (Result "" Success)
