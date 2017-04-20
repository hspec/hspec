module Test.Hspec.Core.Config.Util where

formatOrList :: [String] -> String
formatOrList xs = case xs of
  [] -> ""
  x : ys -> (case ys of
    [] -> x
    _ : [] -> x ++ " or "
    _ : _ : _ -> x ++ ", ") ++ formatOrList ys
