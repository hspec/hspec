{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Core.Runner.PrintSlowSpecItems (
  SlowItem(..),
  printSlowSpecItems
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Format

import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Formatters.V2 (formatLocation)

data SlowItem = SlowItem {
  location :: Maybe Location
, path :: Path
, duration :: Int
}

printSlowSpecItems :: (SlowItem -> Bool) -> Int -> Format -> Format
printSlowSpecItems userFilter n format event = do
  format event
  case event of
    Done items -> do
      let xs = slowItems n userFilter $ map toSlowItem items
      unless (null xs) $ do
        putStrLn "\nSlow spec items:"
        mapM_ printSlowSpecItem xs
    _ -> return ()

toSlowItem :: (Path, Item) -> SlowItem
toSlowItem (path, item) = SlowItem (itemLocation item)  path (toMilliseconds $ itemDuration item)

slowItems :: Int -> (SlowItem -> Bool) -> [SlowItem] -> [SlowItem]
slowItems n userFilter = take n . reverse . sortOn duration . filter (\slowItem -> duration slowItem /= 0 && userFilter slowItem)

printSlowSpecItem :: SlowItem -> IO ()
printSlowSpecItem SlowItem{..} = do
  putStrLn $ "  " ++ maybe "" formatLocation location ++ joinPath path ++ " (" ++ show duration ++ "ms)"
