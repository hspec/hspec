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

toSpecResult :: Bool -> [(Path, Format.Item)] -> SpecResult
toSpecResult failOnEmpty results = SpecResult items success
  where
    items = map toResultItem results
    success = not (failOnEmpty && null results) && all (not . resultItemIsFailure) items

toResultItem :: (Path, Format.Item) -> ResultItem
toResultItem (path, item) = ResultItem path status
  where
    status = case Format.itemResult item of
      Format.Success{} -> ResultItemSuccess
      Format.Pending{} -> ResultItemPending
      Format.Failure{} -> ResultItemFailure
