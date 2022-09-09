module MyFormatter (use, register, formatter, module Api) where

import Data.List
import Test.Hspec.Api.Format.V1 as Api

-- | Make `formatter` available for use with @--format@ and use it by default.
use :: SpecWith a -> SpecWith a
use = (modifyConfig (useFormatter formatter) >>)

-- | Make `formatter` available for use with @--format@.
register :: SpecWith a -> SpecWith a
register = (modifyConfig (registerFormatter formatter) >>)

formatter :: (String, FormatConfig -> IO Format)
formatter = ("my-formatter", \ _config -> return format)

format :: Format
format event = case event of
  ItemDone path item -> putStrLn (formatItem path item)
  _ -> return ()
  where
    formatItem :: Path -> Item -> String
    formatItem path item = joinPath path <> " [" <> formatResult item <> "]"

    formatResult :: Item -> String
    formatResult item = case itemResult item of
      Success {} -> "✔"
      Pending {} -> "‐"
      Failure {} -> "✘"

    joinPath :: Path -> String
    joinPath (groups, requirement) = intercalate " ❯ " $ groups ++ [requirement]
