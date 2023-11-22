{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Test.Hspec.Core.Extension.Config.Type (
  Option(..)
, Config(..)

, setAnnotation
, getAnnotation

, addSpecTransformation
, applySpecTransformation
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified GetOpt.Declarative as Declarative

import           Test.Hspec.Core.Config.Definition (Config(..))
import qualified Test.Hspec.Core.Config.Definition as Core

import           Test.Hspec.Core.Extension.Tree (SpecTree)

newtype Option = Option { unOption :: Declarative.Option Config }

setAnnotation :: Typeable value => value -> Config -> Config
setAnnotation = Core.setConfigAnnotation

getAnnotation :: Typeable value => Config -> Maybe value
getAnnotation = Core.getConfigAnnotation

newtype SpecTransformation = SpecTransformation { unSpecTransformation :: Config -> [SpecTree] -> [SpecTree] }

setSpecTransformation :: (Config -> [SpecTree] -> [SpecTree]) -> Config -> Config
setSpecTransformation = setAnnotation . SpecTransformation

getSpecTransformation :: Config -> Config -> [SpecTree] -> [SpecTree]
getSpecTransformation = maybe (\ _ -> id) unSpecTransformation . getAnnotation

addSpecTransformation :: (Config -> [SpecTree] -> [SpecTree]) -> Config -> Config
addSpecTransformation f config = setSpecTransformation (\ c -> f c . getSpecTransformation config c) config

applySpecTransformation :: Core.Config -> [SpecTree] -> [SpecTree]
applySpecTransformation config = getSpecTransformation config config
