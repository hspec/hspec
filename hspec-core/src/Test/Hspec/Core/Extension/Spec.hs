module Test.Hspec.Core.Extension.Spec {-# WARNING "This API is experimental." #-} (
  mapItems
) where

import           Prelude ()

import           Test.Hspec.Core.Spec

mapItems :: (Item a -> Item b) -> SpecWith a -> SpecWith b
mapItems = mapSpecItem_
