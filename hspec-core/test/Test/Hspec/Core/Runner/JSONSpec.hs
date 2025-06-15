{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Core.Runner.JSONSpec (spec) where

import           Prelude ()
import           Helper

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Aeson (eitherDecode)
import           Test.HUnit (assertFailure)

import qualified Test.Hspec.Core.Runner.JSON as JSON
import           Data.Yaml.TH

import           Test.Hspec.Core.Format hiding (Location)
import qualified Test.Hspec.Core.Format as Format

spec :: Spec
spec = do
  describe "encodeEvent" $ do
    it "" $ do
      let
        event :: Event
        event = ItemDone path item

        path :: Path
        path = (["Prelude.head"], "returns the first element of a list")

        item :: Item
        item = Item {
          itemLocation
        , itemDuration
        , itemInfo
        , itemResult
        } where

        itemLocation :: Maybe Format.Location
        itemLocation = Just $ Format.Location "Foo.hs" 22 9

        itemDuration :: Seconds
        itemDuration = 0.1

        itemInfo :: String
        itemInfo = "" -- FIXME

        itemResult :: Result
        itemResult = Failure (Just location) reason
          where
            location :: Format.Location
            location = Format.Location "Foo.hs" 23 15

            reason :: FailureReason
            reason = ExpectedButGot Nothing expected actual

            expected :: String
            expected = "23"

            actual :: String
            actual = "42"
        
      let
        json = Builder.toLazyByteString (JSON.encodeEvent event)

      print json

      value :: Value <- either assertFailure return . eitherDecode $ json

      value `shouldBe` [yamlQQ|
        status: failure
        reason:
          rendered: |-
            expected: 23
             but got: 42
          location:
            file: Foo.hs
            line: 23
            column: 15
        |]
