{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Test.Hspec.Core.Extension.Config {-# WARNING "This API is experimental." #-} (
-- * Types
  Config(..)
, Path
, ColorMode(..)
, UnicodeMode(..)

-- * Operations
, setAnnotation
, getAnnotation
, addSpecTransformation
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Config.Definition (ColorMode(..), UnicodeMode(..))

import           Test.Hspec.Core.Extension.Config.Type
import           Test.Hspec.Core.Extension.Tree (SpecTree)

addSpecTransformation :: (Config -> [SpecTree] -> [SpecTree]) -> Config -> Config
addSpecTransformation f config = setSpecTransformation (\ c -> f c . getSpecTransformation config c) config
