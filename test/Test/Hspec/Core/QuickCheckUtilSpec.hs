{-# LANGUAGE CPP #-}
module Test.Hspec.Core.QuickCheckUtilSpec (main, spec) where

import           Helper

import           Test.QuickCheck
import           Test.Hspec.Core.QuickCheckUtil

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "formatNumbers" $ do
    it "includes number of tests" $ do
      formatNumbers (failure 1 0) `shouldBe` "(after 1 test)"

    it "pluralizes number of tests" $ do
      formatNumbers (failure 3 0) `shouldBe` "(after 3 tests)"

    it "includes number of shrinks" $ do
      formatNumbers (failure 3 1) `shouldBe` "(after 3 tests and 1 shrink)"

    it "pluralizes number of shrinks" $ do
      formatNumbers (failure 3 3) `shouldBe` "(after 3 tests and 3 shrinks)"
  where
    failure tests shrinks = Failure {
      numTests = tests
    , numShrinks = shrinks
    , numShrinkTries = undefined
    , numShrinkFinal = undefined
    , usedSeed = undefined
    , usedSize = undefined
    , reason = undefined
#if MIN_VERSION_QuickCheck(2,7,0)
    , theException = Nothing
#endif
    , labels = undefined
    , output = undefined
    }
