module Main (main, spec) where

import Prelude
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Runner
import Test.Hspec.Expectations

import Helper (green)

main :: IO ()
main = hspec spec

data Foo = Foo

instance Example Foo where
  evaluateExample _ _ _ _ = return Result {
      resultInfo = "info"
    , resultStatus = Failure Nothing . Reason $ "some " <> green "colorized" <> " error message"
    }

spec :: Spec
spec = do
  describe "reverse" $ do
    it "reverses a list" Foo
