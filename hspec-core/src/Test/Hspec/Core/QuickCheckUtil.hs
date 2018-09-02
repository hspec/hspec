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
import           Test.QuickCheck.Text (isOneLine)
import qualified Test.QuickCheck.Property as QCP
import           Test.QuickCheck.Property hiding (Result(..))
import           Test.QuickCheck.Gen
import           Test.QuickCheck.IO ()
import           Test.QuickCheck.Random

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
#if MIN_VERSION_QuickCheck(2,11,0)
      colonNewline = ":\n"
#else
      colonNewline = ": \n"
#endif

  GaveUp {..} ->
    case stripSuffix outputWithoutVerbose output of
      Just info -> otherFailure info ("Gave up after " ++ pluralize numTests "test" ++ "!")
      Nothing -> couldNotParse output
    where
      outputWithoutVerbose = "*** Gave up! Passed only " ++ pluralize numTests "test" ++ ".\n"

  NoExpectedFailure {..} -> case splitBy "*** Failed! " output of
    Just (info, err) -> otherFailure info err
    Nothing -> couldNotParse output

#if !MIN_VERSION_QuickCheck(2,12,0)
  InsufficientCoverage {..} -> case splitBy ("*** " ++ pre) output of
    Just (info, err) -> otherFailure info (pre ++ err)
    Nothing -> couldNotParse output
    where
      pre = "Insufficient coverage after "
#endif

  where
    result = QuickCheckResult (numTests r) . strip
    otherFailure info err = result info (QuickCheckOtherFailure $ strip err)
    couldNotParse = result "" . QuickCheckOtherFailure

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
