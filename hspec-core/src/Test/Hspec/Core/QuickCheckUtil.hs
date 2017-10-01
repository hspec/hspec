{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Core.QuickCheckUtil where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Exception
import           Data.List
import           Data.Maybe
import           Data.Int
import           System.Random
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as QCP
import           Test.QuickCheck.Property hiding (Result(..))
import           Test.QuickCheck.Gen
import           Test.QuickCheck.IO ()
import           Test.QuickCheck.Random

import           Test.Hspec.Core.Util

import           Test.QuickCheck.Test (formatLabel)

formatLabels :: Int -> [(String, Double)] -> String
formatLabels n = unlines . map (formatLabel n True)

aroundProperty :: ((a -> IO ()) -> IO ()) -> (a -> Property) -> Property
aroundProperty action p = MkProperty . MkGen $ \r n -> aroundProp action $ \a -> (unGen . unProperty $ p a) r n

aroundProp :: ((a -> IO ()) -> IO ()) -> (a -> Prop) -> Prop
aroundProp action p = MkProp $ aroundRose action (\a -> unProp $ p a)

aroundRose :: ((a -> IO ()) -> IO ()) -> (a -> Rose QCP.Result) -> Rose QCP.Result
aroundRose action r = ioRose $ do
  ref <- newIORef (return QCP.succeeded)
  action $ \a -> reduceRose (r a) >>= writeIORef ref
  readIORef ref

newSeed :: IO Int
newSeed = fst . randomR (0, fromIntegral (maxBound :: Int32)) <$>
  newQCGen

mkGen :: Int -> QCGen
mkGen = mkQCGen

formatNumbers :: Int -> Int -> String
formatNumbers n shrinks = "(after " ++ pluralize n "test" ++ shrinks_ ++ ")"
  where
    shrinks_
      | shrinks > 0 = " and " ++ pluralize shrinks "shrink"
      | otherwise = ""

data QuickCheckResult = QuickCheckResult {
  quickCheckResultNumTests :: Int
, quickCheckResultStatus :: Status
} deriving Show

data Status =
    QuickCheckSuccess String
  | QuickCheckFailure QuickCheckFailure
  | QuickCheckOtherFailure String
  deriving Show

data QuickCheckFailure = QCFailure {
  quickCheckFailureNumShrinks :: Int
, quickCheckFailureException :: Maybe SomeException
, quickCheckFailureReason :: String
, quickCheckFailureCounterexample :: String
} deriving Show

parseQuickCheckResult :: Result -> QuickCheckResult
parseQuickCheckResult r = case r of
  Success {..} -> result (QuickCheckSuccess  $ formatLabels numTests labels)

  Failure {..} -> case mCounterexample of
    Nothing -> otherFailure
    Just c -> result (QuickCheckFailure $ QCFailure numShrinks theException reason c)
    where
      numbers = formatNumbers numTests numShrinks
      mCounterexample = maybeStripSuffix "\n" <$> (stripPrefix prefix1 output <|> stripPrefix prefix2 output)
      prefix1 = "*** Failed! " ++ reason ++ " " ++ numbers ++ ": \n"
      prefix2 = "*** Failed! " ++ numbers ++ ": \n" ++ ensureTrailingNewline reason

  GaveUp {..} -> result (QuickCheckOtherFailure $ "Gave up after " ++ pluralize numTests "test")
  NoExpectedFailure {..} -> result (QuickCheckOtherFailure $ "Passed " ++ pluralize numTests "test" ++ " (expected failure)")
  InsufficientCoverage {..} -> otherFailure
  where
    otherFailure = result (QuickCheckOtherFailure $ maybeStripPrefix "*** " . strip $ output r)
    result = QuickCheckResult (numTests r)

ensureTrailingNewline :: String -> String
ensureTrailingNewline = unlines . lines

maybeStripPrefix :: String -> String -> String
maybeStripPrefix prefix m = fromMaybe m (stripPrefix prefix m)

maybeStripSuffix :: String -> String -> String
maybeStripSuffix suffix = reverse . maybeStripPrefix suffix . reverse
