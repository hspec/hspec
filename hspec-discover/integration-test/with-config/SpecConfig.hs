module SpecConfig where

import Test.Hspec.Runner
import Test.Hspec.Core.Formatters.V2

config :: Config -> IO Config
config c = return c {
  configFormat = Just $ formatterToFormat progress
}
