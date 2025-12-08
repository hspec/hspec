{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Hspec.Discover {-# WARNING
  "This module is used by @hspec-discover@.  It is not part of the public API and may change at any time."
  #-} (
  Spec
, hspec
#ifdef ENABLE_LEGACY_V1_FORMATTERS
, IsFormatter (..)
, hspecWithFormatter
#endif
, postProcessSpec
, describe
, module Prelude
) where

import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Runner
#ifdef ENABLE_LEGACY_V1_FORMATTERS
import           Test.Hspec.Core.Formatters.V1

class IsFormatter a where
  toFormatter :: a -> IO Formatter

instance IsFormatter (IO Formatter) where
  toFormatter = id

instance IsFormatter Formatter where
  toFormatter = return

hspecWithFormatter :: IsFormatter a => a -> Spec -> IO ()
hspecWithFormatter formatter spec = do
  f <- toFormatter formatter
  hspecWith defaultConfig {configFormatter = Just f} spec
#endif

postProcessSpec :: FilePath -> Spec -> Spec
postProcessSpec _ = id
