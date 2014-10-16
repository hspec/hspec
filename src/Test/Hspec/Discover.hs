module Test.Hspec.Discover {-# WARNING
  "This module is used by @hspec-discover@.  It is not part of the public API and may change at any time."
  #-} (Spec, hspec, hspecWithFormatter, postProcessSpec, describe) where

import           Test.Hspec
import           Test.Hspec.Runner
import           Test.Hspec.Formatters

hspecWithFormatter :: IsFormatter a => a -> Spec -> IO ()
hspecWithFormatter formatter spec = do
  f <- toFormatter formatter
  hspecWith defaultConfig {configFormatter = Just f} spec

postProcessSpec :: FilePath -> Spec -> Spec
postProcessSpec _ = id
