{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE LambdaCase #-}
module Test.Hspec.CI (
  use
, only
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

-- import           Test.Hspec.Api.Extension.V1
-- import qualified Test.Hspec.Api.Extension.V1.Item as Item
-- import qualified Test.Hspec.Api.Extension.V1.Spec as Spec
-- import qualified Test.Hspec.Api.Extension.V1.Tree as Tree
import           Test.Hspec.Core.Extension
import qualified Test.Hspec.Core.Extension.Item as Item
import qualified Test.Hspec.Core.Extension.Spec as Spec
import qualified Test.Hspec.Core.Extension.Tree as Tree
import qualified Test.Hspec.Core.Extension.Config as Config
import qualified Test.Hspec.Core.Extension.Option as Option

data CiOnly = CiOnly

setCiOnly :: Item a -> Item a
setCiOnly = Item.setAnnotation CiOnly

getCiOnly :: Item a -> Maybe CiOnly
getCiOnly = Item.getAnnotation

-- |
-- Do not run the spec items of the given subtree by default; execute them only
-- when the environment variable @CI@ is set.
--
-- It is idiomatic to use this in combination with `>>>`:
--
-- @
-- import Control.Arrow ((>>>))
-- import Test.Hspec
-- import qualified Test.Hspec.CI as CI
--
-- spec :: Spec
-- spec = do
--   describe ".." $ do
--     it ".." >>> CI.only $ do
--       ..
-- @
only :: SpecWith a -> SpecWith a
only = Spec.mapItems setCiOnly

newtype CiFlag = CI Bool

setCiFlag :: Bool -> Config -> Config
setCiFlag = Config.setAnnotation . CI

getCiFlag :: Config -> CiFlag
getCiFlag = fromMaybe (CI False) . Config.getAnnotation


use :: SpecWith a
use = do
  runIO (lookupEnv "CI") >>= \ case
    Nothing -> pass
    Just _ -> modifyConfig (setCiFlag True)
  registerOptions [
      Option.flag "ci" setCiFlag "include itmes that are marked as \"CI only\""
    ]
  registerTransformation applyCiOnly

applyCiOnly :: Config -> [SpecTree] -> [SpecTree]
applyCiOnly config = case getCiFlag config of
  CI True -> id
  CI False -> Tree.mapItems $ \ item -> case getCiOnly item of
    Nothing -> item
    Just CiOnly -> Item.pendingWith message item
  where
    message = unlines [
        "This item is marked as \"CI only\" and excluded by default."
      , "Use `--ci' to include this item."
      ]
