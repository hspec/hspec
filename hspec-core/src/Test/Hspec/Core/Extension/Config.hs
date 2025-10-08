module Test.Hspec.Core.Extension.Config {-# WARNING in "x-experimental" "This API is experimental." #-} (
-- * Types
  Config(..)
, Path
, ColorMode(..)
, UnicodeMode(..)

-- * Operations
, setAnnotation
, getAnnotation
) where

import           Prelude ()

import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Config.Definition (ColorMode(..), UnicodeMode(..))

import           Test.Hspec.Core.Extension.Config.Type
