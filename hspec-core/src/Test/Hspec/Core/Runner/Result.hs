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

data SpecResult = SpecResult {
  specResultItems :: [ResultItem]
, specResultSuccess :: !Bool
} deriving (Eq, Show)

data ResultItem = ResultItem {
  resultItemPath :: Path
, resultItemStatus :: ResultItemStatus
} deriving (Eq, Show)

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
