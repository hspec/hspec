module Test.Hspec.Core.QuickCheck.Options (
  QuickCheckOptions(..)

, toQuickCheckArgs

, getMaxSuccess
, setMaxSuccess
, modifyMaxSuccess

, getMaxDiscardRatio
, setMaxDiscardRatio
, modifyMaxDiscardRatio

, getMaxSize
, setMaxSize
, modifyMaxSize

, getMaxShrinks
, setMaxShrinks
, modifyMaxShrinks
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.QuickCheck (Args, stdArgs)
import qualified Test.QuickCheck as QC (Args(..))

import           Test.Hspec.Core.QuickCheck.Util
import           Test.Hspec.Api.Example.V1.Options

data QuickCheckOptions = QuickCheckOptions {
  maxSuccess :: Maybe Int
, maxDiscardRatio :: Maybe Int
, maxSize :: Maybe Int
, maxShrinks :: Maybe Int
, modifyArgs :: Args -> Args
}

instance Options QuickCheckOptions where
  defaultOptions = QuickCheckOptions {
        maxSuccess = Nothing
      , maxDiscardRatio = Nothing
      , maxSize = Nothing
      , maxShrinks = Nothing
      , modifyArgs = id
      }
  optionsParser = options "OPTIONS FOR QUICKCHECK" [
      option (Just 'a') "qc-max-success" "N" (withRead setMaxSuccess)
        "maximum number of successful tests before a QuickCheck property succeeds"
    , option Nothing "qc-max-discard" "N" (withRead setMaxDiscardRatio)
        "maximum number of discarded tests per successful test before giving up"
    , option Nothing "qc-max-size" "N" (withRead setMaxSize)
        "size to use for the biggest test cases"
    , option Nothing "qc-max-shrinks" "N" (withRead setMaxShrinks)
        "maximum number of shrinks to perform before giving up (a value of 0 turns shrinking off)"
    ]

toQuickCheckArgs :: Integer -> QuickCheckOptions -> Args
toQuickCheckArgs seed opts = modifyArgs opts stdArgs {
  QC.replay = Just (mkGen (fromInteger seed), 0)
, QC.maxSuccess = getMaxSuccess opts
, QC.maxDiscardRatio = getMaxDiscardRatio opts
, QC.maxSize = getMaxSize opts
, QC.maxShrinks = getMaxShrinks opts
}

getMaxSuccess :: QuickCheckOptions -> Int
getMaxSuccess = get QC.maxSuccess maxSuccess

getMaxDiscardRatio :: QuickCheckOptions -> Int
getMaxDiscardRatio = get QC.maxDiscardRatio maxDiscardRatio

getMaxSize :: QuickCheckOptions -> Int
getMaxSize = get QC.maxSize maxSize

getMaxShrinks :: QuickCheckOptions -> Int
getMaxShrinks = get QC.maxShrinks maxShrinks

get :: (Args -> Int) -> (QuickCheckOptions -> Maybe Int) -> QuickCheckOptions -> Int
get fallback field = fromMaybe (fallback stdArgs) . field

setMaxSuccess :: Int -> QuickCheckOptions -> QuickCheckOptions
setMaxSuccess n args = args {maxSuccess = Just n}

setMaxDiscardRatio :: Int -> QuickCheckOptions -> QuickCheckOptions
setMaxDiscardRatio n args = args {maxDiscardRatio = Just n}

setMaxSize :: Int -> QuickCheckOptions -> QuickCheckOptions
setMaxSize n args = args {maxSize = Just n}

setMaxShrinks :: Int -> QuickCheckOptions -> QuickCheckOptions
setMaxShrinks n args = args {maxShrinks = Just n}

modifyMaxSuccess :: (Int -> Int) -> QuickCheckOptions -> QuickCheckOptions
modifyMaxSuccess = modifyWith getMaxSuccess setMaxSuccess

modifyMaxDiscardRatio :: (Int -> Int) -> QuickCheckOptions -> QuickCheckOptions
modifyMaxDiscardRatio = modifyWith getMaxDiscardRatio setMaxDiscardRatio

modifyMaxSize :: (Int -> Int) -> QuickCheckOptions -> QuickCheckOptions
modifyMaxSize = modifyWith getMaxSize setMaxSize

modifyMaxShrinks :: (Int -> Int) -> QuickCheckOptions -> QuickCheckOptions
modifyMaxShrinks = modifyWith getMaxShrinks setMaxShrinks
