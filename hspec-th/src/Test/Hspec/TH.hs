{-# LANGUAGE TemplateHaskell #-}
module Test.Hspec.TH (
  it
, module Test.Hspec
) where

import           Language.Haskell.TH hiding (location)
import qualified Language.Haskell.TH as TH

import           Test.Hspec hiding (it)
import qualified Test.Hspec as Hspec
import           Test.Hspec.Core (Location(..), LocationAccuracy(..), Item(..), mapSpecItem_)

it :: Q Exp
it = do
  loc <- TH.location
  let filename = loc_filename loc
      (line, column) = loc_start loc
  [|itLoc (Location filename line column ExactLocation)|]

itLoc :: Example a => Location -> String -> a -> SpecWith (Arg a)
itLoc loc r = addLocation . Hspec.it r
  where
    addLocation = mapSpecItem_ (\item -> item {itemLocation = Just loc})
