module Test.Hspec.Core.Extension.Tree (
  SpecTree
, mapItems
, filterItems
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.Hspec.Core.Spec hiding (SpecTree)
import qualified Test.Hspec.Core.Spec as Core

type SpecTree = Core.SpecTree ()

mapItems :: (Item () -> Item ()) -> [SpecTree] -> [SpecTree]
mapItems = map . fmap

filterItems :: (Item () -> Bool) -> [SpecTree] -> [SpecTree]
filterItems = filterForest
