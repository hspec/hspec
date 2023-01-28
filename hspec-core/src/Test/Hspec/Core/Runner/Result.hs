{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Runner.Result (
-- RE-EXPORTED from Test.Hspec.Core.Runner
  SpecResult(SpecResult)
, specResultItems
, specResultSuccess

, ResultItem(ResultItem)
, resultItemPath
, resultItemStatus
, resultItemIsFailure

, ResultItemStatus(..)

, Summary
, summaryExamples
, summaryFailures
, toSummary
, isSuccess
-- END RE-EXPORTED from Test.Hspec.Core.Runner

, toSpecResult
, toSummary_
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.Hspec.Core.Util
import qualified Test.Hspec.Core.Format as Format

-- |
-- @since 2.10.0
data SpecResult = SpecResult {
  -- |
  -- @since 2.10.0
  specResultItems :: [ResultItem]

  -- |
  -- @since 2.10.0
, specResultSuccess :: !Bool
} deriving (Eq, Show)

-- |
-- @since 2.10.0
data ResultItem = ResultItem {
  -- |
  -- @since 2.10.0
  resultItemPath :: Path

  -- |
  -- @since 2.10.0
, resultItemStatus :: ResultItemStatus
} deriving (Eq, Show)

-- |
-- @since 2.11.0
instance Monoid SpecResult where
  mempty = mkSpecResult []
#if MIN_VERSION_base(4,11,0)
-- |
-- @since 2.11.0
instance Semigroup SpecResult where
#endif
  r1
#if MIN_VERSION_base(4,11,0)
    <>
#else
    `mappend`
#endif
    r2 = mkSpecResult (specResultItems r1 <> specResultItems r2)

mkSpecResult :: [ResultItem] -> SpecResult
mkSpecResult items = SpecResult items success
  where
    success = all (not . resultItemIsFailure) items

-- |
-- @since 2.10.0
resultItemIsFailure :: ResultItem -> Bool
resultItemIsFailure item = case resultItemStatus item of
  ResultItemSuccess -> False
  ResultItemPending -> False
  ResultItemFailure -> True

data ResultItemStatus =
    ResultItemSuccess
  | ResultItemPending
  | ResultItemFailure
  deriving (Eq, Show)

toSpecResult :: [(Path, Format.Item)] -> SpecResult
toSpecResult = mkSpecResult . map toResultItem

toResultItem :: (Path, Format.Item) -> ResultItem
toResultItem (path, item) = ResultItem path status
  where
    status = case Format.itemResult item of
      Format.Success{} -> ResultItemSuccess
      Format.Pending{} -> ResultItemPending
      Format.Failure{} -> ResultItemFailure

-- | Summary of a test run.
type Summary = SpecResult

toSummary_ :: SpecResult -> (Int, Int)
toSummary_ result = (length items, length failures)
  where
    items = specResultItems result
    failures = filter resultItemIsFailure items

toSummary :: SpecResult -> Summary
toSummary = id

-- | `True` if the given `Summary` indicates that there were no
-- failures, `False` otherwise.
isSuccess :: Summary -> Bool
isSuccess = specResultSuccess

summaryExamples :: SpecResult -> Int
summaryExamples = fst . toSummary_

summaryFailures :: SpecResult -> Int
summaryFailures = snd . toSummary_
