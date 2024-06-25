{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Test.Hspec.Core.Extension.Config.Type (
  Option(..)
, Config(..)

, setAnnotation
, getAnnotation

, setSpecTransformation
, getSpecTransformation

, applySpecTransformation
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified GetOpt.Declarative as Declarative

import           Test.Hspec.Core.Config.Definition (Config(..))
import qualified Test.Hspec.Core.Config.Definition as Core
import qualified Test.Hspec.Core.Annotations as Annotations

import           Test.Hspec.Core.Extension.Tree (SpecTree)

newtype Option = Option { unOption :: Declarative.Option Config }

setAnnotation :: Typeable value => value -> Config -> Config
setAnnotation value config = config { configAnnotations = Annotations.setValue value $ configAnnotations config }

getAnnotation :: Typeable value => Config -> Maybe value
getAnnotation = Annotations.getValue . configAnnotations

newtype SpecTransformation = SpecTransformation { unSpecTransformation :: Config -> [SpecTree] -> [SpecTree] }

setSpecTransformation :: (Config -> [SpecTree] -> [SpecTree]) -> Config -> Config
setSpecTransformation = setAnnotation . SpecTransformation

getSpecTransformation :: Config -> Config -> [SpecTree] -> [SpecTree]
getSpecTransformation = maybe (\ _ -> id) unSpecTransformation . getAnnotation

applySpecTransformation :: Core.Config -> [SpecTree] -> [SpecTree]
applySpecTransformation config = getSpecTransformation config config
