{-# LANGUAGE CPP, TypeFamilies, FlexibleInstances, TypeSynonymInstances, DeriveDataTypeable #-}
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
data Result = Success | Pending (Maybe String) | Fail String
  deriving (Eq, Show, Read, Typeable)

instance E.Exception Result

instance Example Bool where
  type Arg Bool = ()
  evaluateExample b _ _ _ = if b then return Success else return (Fail "")

instance Example Expectation where
  type Arg Expectation = ()
  evaluateExample = evaluateExpectation . uncurry0

instance Example (a -> Expectation) where
  type Arg (a -> Expectation) = (a, ())
  evaluateExample = evaluateExpectation . uncurry1

instance Example (a -> b -> Expectation) where
  type Arg (a -> b -> Expectation) = (b, (a, ()))
  evaluateExample = evaluateExpectation . uncurry2

instance Example (a -> b -> c -> Expectation) where
  type Arg (a -> b -> c -> Expectation) = (c, (b, (a, ())))
  evaluateExample = evaluateExpectation . uncurry3

evaluateExpectation :: (a -> Expectation) -> Params -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
evaluateExpectation e _ around _ = ((around e) >> return Success) `E.catches` [
    E.Handler (\(HUnitFailure err) -> return (Fail err))
  , E.Handler (return :: Result -> IO Result)
  ]

instance Example Result where
  type Arg Result = ()
  evaluateExample r _ _ _ = return r

instance Example QC.Property where
  type Arg QC.Property = ()
  evaluateExample = evaluateProperty . uncurry0

instance Example (a -> QC.Property) where
  type Arg (a -> QC.Property) = (a, ())
  evaluateExample = evaluateProperty . uncurry1

instance Example (a -> b -> QC.Property) where
  type Arg (a -> b -> QC.Property) = (b, (a, ()))
  evaluateExample = evaluateProperty . uncurry2

instance Example (a -> b -> c -> QC.Property) where
  type Arg (a -> b -> c -> QC.Property) = (c, (b, (a, ())))
  evaluateExample = evaluateProperty . uncurry3

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

uncurry0 :: r -> () -> r
uncurry0 r () = r

uncurry1 :: (a -> r) -> (a, ()) -> r
uncurry1 action = (\(a, ()) -> action a)

uncurry2 :: (a -> b -> r) -> (b, (a, ())) -> r
uncurry2 action = (\(b, (a, ())) -> action a b)

uncurry3 :: (a -> b -> c -> r) -> (c, (b, (a, ()))) -> r
uncurry3 action = (\(c, (b, (a, ()))) -> action a b c)
