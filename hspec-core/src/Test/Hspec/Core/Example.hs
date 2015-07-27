{-# LANGUAGE CPP, TypeFamilies, FlexibleInstances, TypeSynonymInstances, DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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

import           Control.DeepSeq
import qualified Control.Exception as E
import           Data.List (isPrefixOf)
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable)
import           Test.HUnit.Lang (HUnitFailure(..))
import           Test.Hspec.Expectations (Expectation)
import qualified Test.QuickCheck as QC

import qualified Test.QuickCheck.Property as QCP
import qualified Test.QuickCheck.State as QC

import           Test.Hspec.Compat
import           Test.Hspec.Core.QuickCheckUtil
import           Test.Hspec.Core.Uncurry
import           Test.Hspec.Core.Util

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

-- | A type class for examples
class Example e where
  type ArgTypes e :: [*]
  evaluateExample :: e -> (((HList (ArgTypes e)) -> IO ()) -> IO ()) -> IO Result
  -- evaluateExample :: e -> HList (ArgTypes e) -> IO Result

data HList (a :: [*]) where
  Nil :: HList '[]
  Cons :: x -> HList xs -> HList (x ': xs)

instance Example Result where
  type ArgTypes Result = '[]
  evaluateExample e aroundWrapper = do
    ref <- newIORef Success
    aroundWrapper $ \Nil -> do
      E.evaluate e >>= writeIORef ref -- FIXME: deepseq result
    readIORef ref
  
--instance Example Bool where
--  type ArgTypes Bool = '[]
--  evaluateExample e = evaluateExample (if e then Success else Fail "")

instance (Example e) => Example (a -> e) where
  type ArgTypes (a -> e) = a ': ArgTypes e
  evaluateExample e aroundWrapper = do
    ref <- newIORef Success
    aroundWrapper $ \(Cons x xs) ->
      evaluateExample _ (\action -> action xs) >>= writeIORef ref
    readIORef ref

instance Example Expectation where
  type ArgTypes Expectation = '[]
  evaluateExample e aroundWrapper = aroundWrapper (\Nil -> e) >> return Success -- fixme: handle exceptions


{-
instance Testable Expectation where
  evaluateTestable = evaluateExpectation

evaluateExpectation :: (a -> Expectation) -> Params -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
evaluateExpectation e _ around _ = ((around e) >> return Success) `E.catches` [
    E.Handler (\(HUnitFailure err) -> return (Fail err))
  , E.Handler (return :: Result -> IO Result)
  ]
instance Example QC.Property where
  type Arg QC.Property = ()
  type T QC.Property = QC.Property
  toSimple p = \ () -> p

instance Testable QC.Property where
  evaluateTestable = evaluateProperty

evaluateExample :: (a -> b -> QC.Property)  -> HList (ArgTypes e) -> IO Result

evaluateProperty :: (a -> QC.Property) -> Params -> ((a -> IO ()) -> IO ()) -> ProgressCallback -> IO Result
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
-}
