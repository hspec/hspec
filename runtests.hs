import Test.Hspec
import Specs (specs)

main :: IO ()
main = specs >>= hspecX
