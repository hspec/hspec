import Test.Hspec
import Control.Exception
import Control.Concurrent

main :: IO ()
main = hspec $
  describe "it" $ do
    it "should run" $ do
      (threadDelay 10000000 >> putStrLn "ran action")
        `finally` (threadDelay 1000000 >> putStrLn "ran finalizer")
