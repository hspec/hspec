{-# LANGUAGE LambdaCase #-}
module Test.Hspec.CI (
  use
, only
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.Hspec.Core.Extension

data CiOnly = CiOnly

setCiOnly :: Item a -> Item a
setCiOnly = setItemAnnotation CiOnly

getCiOnly :: Item a -> Maybe CiOnly
getCiOnly = getItemAnnotation

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
only = mapSpecItem setCiOnly

newtype CiFlag = CI Bool

setCiFlag :: Bool -> Config -> Config
setCiFlag = setConfigAnnotation . CI

getCiFlag :: Config -> CiFlag
getCiFlag = fromMaybe (CI False) . getConfigAnnotation

ciFlag :: Option
ciFlag = flag "ci" setCiFlag "XXXXXXXXXX TODO XXXXXXXXXXXXXXXX"

use :: SpecWith a
use = do
  runIO (lookupEnv "CI") >>= \ case
    Nothing -> pass
    Just _ -> modifyConfig (setCiFlag True)
  registerOption "hspec-ci" ciFlag
  addTransformation applyTagsToSpec

applyTagsToSpec :: Config -> [SpecTree ()] -> [SpecTree ()]
applyTagsToSpec config = case getCiFlag config of
  CI True -> id
  CI False -> mapItems $ \ item -> case getCiOnly item of
    Nothing -> item
    Just CiOnly -> setItemPending (Just "XXXXXXXXXXXXXX TODO ci-only XXXXXXXXXXXXXXX") item
