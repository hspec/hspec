module Test.Hspec.Core.TimerSpec (main, spec) where

import           Helper

import           Test.Hspec.Core.Timer

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "timer action provided by withTimer" $ do

    let
      dt = 0.01
      wait = sleep (dt * 1.2)

    it "returns False" $ do
      withTimer dt $ \timer -> do
        timer `shouldReturn` False

    context "after specified time" $ do
      it "returns True" $ do
        withTimer dt $ \timer -> do
          wait
          timer `shouldReturn` True
          timer `shouldReturn` False
          wait
          wait
          timer `shouldReturn` True
          timer `shouldReturn` False
