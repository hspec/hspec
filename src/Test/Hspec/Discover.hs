-- | This functions are used by @hspec-discover@.  They are not part of the
-- public API and may change at any time.
module Test.Hspec.Discover (Spec, hspec, hspecWithFormatter, postProcessSpec, describe) where

import           Test.Hspec
import           Test.Hspec.Runner
import           Test.Hspec.Formatters

hspecWithFormatter :: IsFormatter a => a -> Spec -> IO ()
hspecWithFormatter formatter spec = do
  f <- toFormatter formatter
  hspecWith defaultConfig {configFormatter = Just f} spec

postProcessSpec :: FilePath -> Spec -> Spec
postProcessSpec _ = id
