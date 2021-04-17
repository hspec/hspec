{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Core.Runner.PrintSlowSpecItemsSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Runner.PrintSlowSpecItems

format :: Format IO
format = Format {
  formatRun = id
, formatGroupStarted = \ _ -> return ()
, formatGroupDone = \ _ -> return ()
, formatProgress = \ _ _ -> return ()
, formatItemStarted = \ _ -> return ()
, formatItemDone = \ _ _ -> return ()
}

location :: Location
location = Location {
  locationFile = "Foo.hs"
, locationLine = 23
, locationColumn = 42
}

item :: Item
item = Item {
  itemLocation = Just location
, itemDuration = 0
, itemInfo = undefined
, itemResult = undefined
}

spec :: Spec
spec = do
  describe "printSlowSpecItems" $ do
    it "prints slow spec items" $ do
      Format{..} <- printSlowSpecItems 2 format
      capture_ $ formatRun $ do
        formatItemDone (["foo", "bar"], "one") item {itemDuration = 0.100}
        formatItemDone (["foo", "bar"], "two") item {itemDuration = 0.500}
        formatItemDone (["foo", "bar"], "thr") item {itemDuration = 0.050}
      `shouldReturn` unlines [
        ""
      , "Slow spec items:"
      , "  Foo.hs:23:42: /foo/bar/two/ (500ms)"
      , "  Foo.hs:23:42: /foo/bar/one/ (100ms)"
      ]

    context "when there are no slow items" $ do
      it "prints nothing" $ do
        Format{..} <- printSlowSpecItems 2 format
        capture_ $ formatRun $ do
          formatItemDone (["foo", "bar"], "one") item {itemDuration = 0}
        `shouldReturn` ""
