{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Core.Runner.PrintSlowSpecItemsSpec (spec) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Runner.PrintSlowSpecItems

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
    let format = printSlowSpecItems 2 $ \ _ -> return ()
    it "prints slow spec items" $ do
      capture_ $ format $ Done [
            ((["foo", "bar"], "one"), item {itemDuration = 0.100})
          , ((["foo", "bar"], "two"), item {itemDuration = 0.500})
          , ((["foo", "bar"], "thr"), item {itemDuration = 0.050})
          ]
      `shouldReturn` unlines [
        ""
      , "Slow spec items:"
      , "  Foo.hs:23:42: /foo/bar/two/ (500ms)"
      , "  Foo.hs:23:42: /foo/bar/one/ (100ms)"
      ]

    context "when there are no slow items" $ do
      it "prints nothing" $ do
        capture_ $ format $ Done [((["foo", "bar"], "one"), item {itemDuration = 0})]
        `shouldReturn` ""
