{-# LANGUAGE FlexibleInstances #-}
module Test.Hspec.Discover {-# WARNING
  "This module is used by @hspec-discover@.  It is not part of the public API and may change at any time."
  #-} (
  Spec
, hspec
, IsFormatter (..)
, hspecWithFormatter
, postProcessSpec
, describe
, module Prelude
) where

import           Prelude hiding (mapM)

import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Runner
import           Test.Hspec.Formatters

hspecWithFormatter :: IsFormatter a => a -> Spec -> IO ()
hspecWithFormatter formatter spec = do
  let f = toFormatter formatter
  hspecWith defaultConfig {configFormatter = Just f} spec

postProcessSpec :: FilePath -> Spec -> Spec
postProcessSpec _ = id
