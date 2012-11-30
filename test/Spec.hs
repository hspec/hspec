{-# LINE 1 "test/Spec.hs" #-}module Main where
import Test.Hspec
import qualified Test.HspecSpec
import qualified Test.Hspec.CompatSpec
import qualified Test.Hspec.Core.TypeSpec
import qualified Test.Hspec.FailureReportSpec
import qualified Test.Hspec.FormattersSpec
import qualified Test.Hspec.HUnitSpec
import qualified Test.Hspec.QuickCheckSpec
import qualified Test.Hspec.RunnerSpec
import qualified Test.Hspec.UtilSpec
main :: IO ()
main = hspec $ describe "Test.Hspec" Test.HspecSpec.spec >> describe "Test.Hspec.Compat" Test.Hspec.CompatSpec.spec >> describe "Test.Hspec.Core.Type" Test.Hspec.Core.TypeSpec.spec >> describe "Test.Hspec.FailureReport" Test.Hspec.FailureReportSpec.spec >> describe "Test.Hspec.Formatters" Test.Hspec.FormattersSpec.spec >> describe "Test.Hspec.HUnit" Test.Hspec.HUnitSpec.spec >> describe "Test.Hspec.QuickCheck" Test.Hspec.QuickCheckSpec.spec >> describe "Test.Hspec.Runner" Test.Hspec.RunnerSpec.spec >> describe "Test.Hspec.Util" Test.Hspec.UtilSpec.spec
