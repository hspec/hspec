{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Hspec.Core.Example (
-- RE-EXPORTED from Test.Hspec.Core.Spec
  Example (..)
, Arg
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

type Arg e = ()

-- | A type class for examples, that is to say, test bodies as used in
-- `Test.Hspec.Core.Spec.it` and similar functions.
class Example e where
  -- | Evaluates an example.
  --
  -- `evaluateExample` is expected to execute the test body inside the IO action
  -- passed to the hook. It's often necessary to use an `IORef` to pass data
  -- out like whether the test succeeded to the outer IO action so it can be
  -- returned as a `Result`.
  --
  -- Example:
  --
  -- @
  -- newtype MyAction = MyAction (Int -> IO Bool)
  --
  -- instance Example MyAction where
  --   type Arg MyAction = Int
  --
  --   evaluateExample (MyAction act) _params hook _progress = do
  --     result <- newIORef (Result "" Success)
  --     hook $ \arg -> do
  --       -- e.g. determines if arg is 42
  --       ok <- act arg
  --       let result' = Result "" $ if ok then Success else Failure Nothing NoReason
  --       writeIORef result result'
  --     readIORef result
  -- @
  evaluateExample
    :: e
    -- ^ The example being evaluated
    -> Params
    -- ^ QuickCheck/SmallCheck settings
    -> (ActionWith (Arg e) -> IO ())
    -- ^ Hook: takes an @`ActionWith` (`Arg` e)@, namely, the IO action to run
    -- the test body, obtains @`Arg` e@ from somewhere, then executes the test
    -- body (or possibly decides not to execute it!).
    --
    -- This is used to implement `Test.Hspec.Core.Hooks.around` and similar
    -- hook functionality.
    -> ProgressCallback
    -- ^ Callback for composite tests like QuickCheck to report their progress.
    -> IO Result

-- | QuickCheck and SmallCheck related parameters.
data Params = Params {
  paramsQuickCheckArgs  :: QC.Args
, paramsSmallCheckDepth :: Maybe Int
} deriving (Show)

defaultParams :: Params
defaultParams = Params {
  paramsQuickCheckArgs = QC.stdArgs
, paramsSmallCheckDepth = Nothing
}

-- | @(CurrentItem, TotalItems)@ tuple.
type Progress = (Int, Int)
-- | Callback used by composite test items that contain many tests to report
-- their progress towards finishing them all.
--
-- This is used, for example, to report how many QuickCheck examples are finished.
type ProgressCallback = Progress -> IO ()

-- | An `IO` action that expects an argument of type @a@.
--
-- This type is what `Example`s are ultimately unlifted into for execution.
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
  evaluateExample e = evaluateExample (\() -> e)

instance Example (() -> Result) where
  evaluateExample example _params hook _callback = do
    liftHook (Result "" Success) hook (evaluate . example)

instance Example Bool where
  evaluateExample e = evaluateExample (\() -> e)

instance Example (() -> Bool) where
  evaluateExample p _params hook _callback = do
    liftHook (Result "" Success) hook (evaluate . example)
    where
      example a
        | p a = Result "" Success
        | otherwise = Result "" $ Failure Nothing NoReason

instance Example Expectation where
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

instance Example (() -> Expectation) where
  evaluateExample e _params hook _ = hook e >> return (Result "" Success)
