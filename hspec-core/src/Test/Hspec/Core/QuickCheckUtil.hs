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
import           Test.QuickCheck.Text (isOneLine)
import qualified Test.QuickCheck.Property as QCP
import           Test.QuickCheck.Property hiding (Result(..))
import           Test.QuickCheck.Gen
import           Test.QuickCheck.IO ()
import           Test.QuickCheck.Random
import qualified Test.QuickCheck.Test as QC (showTestCount)
import           Test.QuickCheck.State (State(..))

import           Test.Hspec.Core.Util

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
, quickCheckResultInfo :: String
, quickCheckResultStatus :: Status
} deriving Show

data Status =
    QuickCheckSuccess
  | QuickCheckFailure QuickCheckFailure
  | QuickCheckOtherFailure String
  deriving Show

data QuickCheckFailure = QCFailure {
  quickCheckFailureNumShrinks :: Int
, quickCheckFailureException :: Maybe SomeException
, quickCheckFailureReason :: String
, quickCheckFailureCounterexample :: [String]
} deriving Show

parseQuickCheckResult :: Result -> QuickCheckResult
parseQuickCheckResult r = case r of
  Success {..} -> result output QuickCheckSuccess

  Failure {..} ->
    case stripSuffix outputWithoutVerbose output of
      Just xs -> result verboseOutput (QuickCheckFailure $ QCFailure numShrinks theException reason failingTestCase)
        where
          verboseOutput
            | xs == "*** Failed! " = ""
            | otherwise = maybeStripSuffix "*** Failed!" (strip xs)
      Nothing -> couldNotParse output
    where
      outputWithoutVerbose = reasonAndNumbers ++ unlines failingTestCase
      reasonAndNumbers
        | isOneLine reason = reason ++ " " ++ numbers ++ colonNewline
        | otherwise = numbers ++ colonNewline ++ ensureTrailingNewline reason
      numbers = formatNumbers numTests numShrinks
      colonNewline = ":\n"

  GaveUp {..} ->
    case stripSuffix outputWithoutVerbose output of
      Just info -> otherFailure info ("Gave up after " ++ numbers ++ "!")
      Nothing -> couldNotParse output
    where
      numbers = showTestCount numTests numDiscarded
      outputWithoutVerbose = "*** Gave up! Passed only " ++ numbers ++ " tests.\n"

  NoExpectedFailure {..} -> case splitBy "*** Failed! " output of
    Just (info, err) -> otherFailure info err
    Nothing -> couldNotParse output

  where
    result = QuickCheckResult (numTests r) . strip
    otherFailure info err = result info (QuickCheckOtherFailure $ strip err)
    couldNotParse = result "" . QuickCheckOtherFailure

showTestCount :: Int -> Int -> String
showTestCount success discarded = QC.showTestCount state
  where
    state = MkState {
      terminal                  = undefined
    , maxSuccessTests           = undefined
    , maxDiscardedRatio         = undefined
    , coverageConfidence        = undefined
    , computeSize               = undefined
    , numTotMaxShrinks          = 0
    , numSuccessTests           = success
    , numDiscardedTests         = discarded
    , numRecentlyDiscardedTests = 0
    , labels                    = mempty
    , classes                   = mempty
    , tables                    = mempty
    , requiredCoverage          = mempty
    , expected                  = True
    , randomSeed                = mkGen 0
    , numSuccessShrinks         = 0
    , numTryShrinks             = 0
    , numTotTryShrinks          = 0
    }

ensureTrailingNewline :: String -> String
ensureTrailingNewline = unlines . lines

maybeStripPrefix :: String -> String -> String
maybeStripPrefix prefix m = fromMaybe m (stripPrefix prefix m)

maybeStripSuffix :: String -> String -> String
maybeStripSuffix suffix = reverse . maybeStripPrefix (reverse suffix) . reverse

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix = fmap reverse . stripPrefix (reverse suffix) . reverse

splitBy :: String -> String -> Maybe (String, String)
splitBy sep xs = listToMaybe [
    (x, y) | (x, Just y) <- zip (inits xs) (map stripSep $ tails xs)
  ]
  where
    stripSep = stripPrefix sep
