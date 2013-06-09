module Test.Hspec.TimerSpec (main, spec) where

import           Test.Hspec.Meta
import           Helper

import           Test.Hspec.Timer

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "timer action returned by newTimer" $ do

    let dt = 0.01

    it "returns False" $ do
      timer <- newTimer dt
      timer `shouldReturn` False

    context "after specified time" $ do
      it "returns True" $ do
        timer <- newTimer dt
        sleep dt
        timer `shouldReturn` True
        timer `shouldReturn` False
        sleep dt
        sleep dt
        timer `shouldReturn` True
        timer `shouldReturn` False
