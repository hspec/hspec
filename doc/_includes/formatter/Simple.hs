import Test.Hspec
import Test.Hspec.Runner (hspecWith, defaultConfig)
import Test.QuickCheck (property)

import Data.List (intercalate)
import Test.Hspec.Api.Format.V1

formatter :: (String, FormatConfig -> IO Format)
formatter = ("my-formatter", \ _config -> return format)

format :: Format
format event = case event of
  ItemDone path item -> putStrLn (formatItem path item)
  _ -> return ()
  where
    formatItem :: Path -> Item -> String
    formatItem path item = joinPath path <> " [" <> formatResult item <> "]"

    formatResult :: Item -> String
    formatResult item = case itemResult item of
      Success {} -> "✔"
      Pending {} -> "‐"
      Failure {} -> "✘"

    joinPath :: Path -> String
    joinPath (groups, requirement) = intercalate " ❯ " $ groups ++ [requirement]

main :: IO ()
main = hspecWith (useFormatter formatter defaultConfig) spec

spec :: Spec
spec = do
  describe "reverse" $ do
    it "reverses a list" $ do
      reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    it "gives the original list, if applied twice" $ property $
      \ xs -> (reverse . reverse) xs == (xs :: [Int])
