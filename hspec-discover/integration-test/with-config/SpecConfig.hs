module SpecConfig where

import Test.Hspec.Runner

config :: Config -> IO Config
config c = return c {
  configDryRun = True
}
