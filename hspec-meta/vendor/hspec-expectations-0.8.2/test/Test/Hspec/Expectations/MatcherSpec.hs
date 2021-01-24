module Test.Hspec.Expectations.MatcherSpec (main, spec) where

import           Test.Hspec

import           Test.Hspec.Expectations.Matcher

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "matchList" $ do
    it "succeeds if arguments are empty lists" $ do
      matchList [] ([] :: [Int]) `shouldBe` Nothing

    it "succeeds if arguments are equal up to permutation" $ do
      matchList [1, 2, 2, 3] [3, 2, 1, 2 :: Int] `shouldBe` Nothing

    context "when arguments are not equal up to permutation" $ do
      it "shows extra elements" $ do
        [1, 2, 2, 3] `matchList` [1, 2, 3 :: Int] `shouldBe` (Just . unlines) [
            "Actual list is not a permutation of expected list!"
          , "  expected list contains:   [1, 2, 3]"
          , "  actual list contains:     [1, 2, 2, 3]"
          , "  the extra elements are:   [2]"
          ]

      it "shows missing elements" $ do
        [1, 2, 3] `matchList` [1, 2, 2, 3 :: Int] `shouldBe` (Just . unlines) [
            "Actual list is not a permutation of expected list!"
          , "  expected list contains:   [1, 2, 2, 3]"
          , "  actual list contains:     [1, 2, 3]"
          , "  the missing elements are: [2]"
          ]
