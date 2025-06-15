{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
module Test.Hspec.Core.Runner.JSON where

import           Prelude ()
import           Test.Hspec.Core.Compat
import           Test.Hspec.Core.Util

import           Test.Hspec.Core.Format hiding (Location)
import qualified Test.Hspec.Core.Format as Format
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder

import           Data.List

encodeLocation :: Format.Location -> Builder
encodeLocation (Format.Location file line column) = encodeObject [
    ("file", encodeString file)
  , ("line", Builder.intDec line)
  , ("column", Builder.intDec column)
  ]

encodeString :: String -> Builder
encodeString str = "\"" <> Builder.string8 (escape str) <> "\""
  where
    escape = \ case
      [] -> []
      '\n' : xs -> '\\' : 'n' : escape xs
      x : xs -> x : escape xs

encodeObject :: [(String, Builder)] -> Builder
encodeObject pairs = Builder.char8 '{' <> (mconcat $ intersperse comma (map encodeField pairs)) <> Builder.char8 '}'
  where
    comma = Builder.char8 ','
    encodeField (key, value) = encodeString key <> Builder.char8 ':' <> value

encodeEvent :: Event -> Builder
encodeEvent = \ case
  Started -> undefined
  GroupStarted _ -> undefined
  GroupDone _ -> undefined
  Progress _ _ -> undefined
  ItemStarted _ -> undefined
  ItemDone _ (Item _ _ _ result) -> case result of
    Format.Success -> undefined
    Format.Pending (_ :: Maybe Format.Location) (_ :: Maybe String) -> undefined
    Format.Failure (location :: Maybe Format.Location) (reason :: FailureReason) -> encodeObject [
          ("status", encodeString "failure")
        , ("reason", rs)
        ]
        where
          rs :: Builder
          rs = encodeObject $
            (maybe id (:) lot)
            $ (maybe id (:) bar)
            []

          lot :: Maybe (String, Builder)
          lot = (,) "location" . encodeLocation <$> location

          bar :: Maybe (String, Builder)
          bar = (,) "rendered" . encodeString <$> foo

          foo :: Maybe String
          foo = case reason of
            NoReason -> Nothing
            Format.Reason message -> Just message
            ColorizedReason message -> Just $ stripAnsi message
            ExpectedButGot preface expected actual -> Just $ intercalate "\n" [
                "expected: " <> expected
              , " but got: " <> actual
              ]
            Error (_ :: Maybe String) (_ :: SomeException) -> undefined

  Done _ -> undefined

format :: Event -> IO ()
format = \ case
  Started -> pass
  GroupStarted _ -> pass
  GroupDone _ -> pass
  Progress _ _ -> pass
  ItemStarted _ -> pass
  ItemDone _ _ -> pass
  Done _ -> pass

{-
data ItemFailed = ItemFailed {
  status :: Status
, location :: Maybe Location
, describe :: [String]
, it :: String
, reason :: Reason
, duration :: Seconds
}

data Status =
    Success
  | Pending
  | Failure

data Location = Location {
  file :: FilePath
, line :: Int
, column :: Int
}

data Reason = Reason {
}
-}


{-
data Item = Item {
  itemLocation :: Maybe Location
, itemDuration :: Seconds
, itemInfo :: String
, itemResult :: Result
} deriving Show
-}

{-
  "event": "example",
  "status": "failure",  // or "success", "pending"
  "description": ["MyModule", "foo", "does something"],
  "location": {
    "file": "test/MyModuleSpec.hs",
    "line": 17,
    "column": 7
  },
  "duration": 0.003,
  "reason": {
    "expected": "42",
    "actual": "0"
  }
-}
