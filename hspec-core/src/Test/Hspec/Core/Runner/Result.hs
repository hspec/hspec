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

, Summary(..)
, toSummary
, isSuccess
-- END RE-EXPORTED from Test.Hspec.Core.Runner

, toSpecResult
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
toSpecResult results = SpecResult items success
  where
    items = map toResultItem results
    success = all (not . resultItemIsFailure) items

toResultItem :: (Path, Format.Item) -> ResultItem
toResultItem (path, item) = ResultItem path status
  where
    status = case Format.itemResult item of
      Format.Success{} -> ResultItemSuccess
      Format.Pending{} -> ResultItemPending
      Format.Failure{} -> ResultItemFailure

-- | Summary of a test run.
data Summary = Summary {
  summaryExamples :: !Int
, summaryFailures :: !Int
} deriving (Eq, Show)

instance Monoid Summary where
  mempty = Summary 0 0
#if MIN_VERSION_base(4,11,0)
instance Semigroup Summary where
#endif
  (Summary x1 x2)
#if MIN_VERSION_base(4,11,0)
    <>
#else
    `mappend`
#endif
    (Summary y1 y2) = Summary (x1 + y1) (x2 + y2)

toSummary :: SpecResult -> Summary
toSummary result = Summary {
  summaryExamples = length items
, summaryFailures = length failures
} where
    items = specResultItems result
    failures = filter resultItemIsFailure items

-- | `True` if the given `Summary` indicates that there were no
-- failures, `False` otherwise.
isSuccess :: Summary -> Bool
isSuccess summary = summaryFailures summary == 0
