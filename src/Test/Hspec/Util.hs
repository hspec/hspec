module Test.Hspec.Util (quantify) where

-- | Create a more readable display of a quantity of something.
--
-- Examples:
--
-- >>> quantify 0 "example"
-- "0 examples"
--
-- >>> quantify 1 "example"
-- "1 example"
--
-- >>> quantify 2 "example"
-- "2 examples"
quantify :: Int -> String -> String
quantify 1 s = "1 " ++ s
quantify n s = show n ++ " " ++ s ++ "s"
