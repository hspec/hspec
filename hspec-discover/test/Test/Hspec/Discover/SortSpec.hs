module Test.Hspec.Discover.SortSpec (spec) where

import           Helper
import           Test.QuickCheck

import           Test.Hspec.Discover.Sort

shuffleAndSort :: [String] -> IO [String]
shuffleAndSort xs = sortNaturally <$> generate (shuffle xs)

sortNaturally :: [String] -> [String]
sortNaturally = sortNaturallyBy $ \ name -> (name, 0)

spec :: Spec
spec = do
  describe "naturalSortKey" $ do
    it "is injective" $ property $ \ a  b -> do
      a /= b ==> naturalSortKey a /= naturalSortKey b

  describe "sortNaturally" $ do
    it "gives shorter strings precedence" $ do
      let expected = [
              ""
            , "a"
            , "aa"
            ]
      shuffleAndSort expected `shouldReturn` expected

    it "gives numbers precedence" $ do
      let expected = [
              "Hello2World"
            , "Hello World"
            ]
      shuffleAndSort expected `shouldReturn` expected

    it "sorts numbers in ascending order" $ do
      let expected = [
              "Spec9.hs"
            , "Spec10.hs"
            ]
      shuffleAndSort expected `shouldReturn` expected

    it "breaks numeric ties by string length" $ do
      let expected = [
              "Hello 2 World"
            , "Hello 02 World"
            ]
      shuffleAndSort expected `shouldReturn` expected

    it "given upper-case letters precedence over lower-case letters" $ do
      let
        expected = [
            "AA.hs"
          , "Aa.hs"
          , "aA.hs"
          , "aa.hs"
          , "B.hs"
          , "b.hs"
          ]
      shuffleAndSort expected `shouldReturn` expected

    it "sorts number separated strings" $ do
      let expected = [
              "Hello2World9"
            , "Hello2World!0"
            ]
      shuffleAndSort expected `shouldReturn` expected

    it "sorts string separated numbers" $ do
      let expected = [
              "3.1.415"
            , "3.14.15"
            ]
      shuffleAndSort expected `shouldReturn` expected

    it "groups common string prefixes together" $ do
      let expected = [
              "SpecFoo.hs"
            , "SpecFoo.lhs"
            , "Specfoo.hs"
            , "Specfoo.lhs"
            ]
      shuffleAndSort expected `shouldReturn` expected
