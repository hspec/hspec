import System.Environment
import Test.Hspec

main :: IO ()
main = withProgName "Spec.hs" . hspec $ do
  return ()
