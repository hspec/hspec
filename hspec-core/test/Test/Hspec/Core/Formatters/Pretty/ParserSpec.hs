module Test.Hspec.Core.Formatters.Pretty.ParserSpec (spec, User(..)) where

import           Prelude ()
import           Helper

import           Test.Hspec.Core.Formatters.Pretty.Parser

data User = User {
  firstName :: String
, lastName :: String
, age :: Int
} deriving (Eq, Show)

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses lists" $ do
      let input = show $ [1, 2, 3 :: Int]
      parser input `shouldBe` Just (List ["1", "2", "3"])

    it "parses records" $ do
      let input = show $ User "John" "Doe" 23
      parser input `shouldBe` Just (Record "User" [("firstName", show "John"), ("lastName", show "Doe"), ("age", "23")])
