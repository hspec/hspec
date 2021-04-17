{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Core.Runner.PrintSlowSpecItems (
  printSlowSpecItems
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Format

import           Test.Hspec.Core.Clock
import           Control.Monad.IO.Class
import           Test.Hspec.Core.Formatters (formatLocation)

data SlowItem = SlowItem {
  location :: Maybe Location
, path :: Path
, duration :: Int
}

printSlowSpecItems :: MonadIO m => Int -> Format m -> IO (Format m)
printSlowSpecItems n format = printSlowSpecItems_ <$> newIORef [] <*> pure n <*> pure format

printSlowSpecItems_ :: MonadIO m => IORef [SlowItem] -> Int -> Format m -> Format m
printSlowSpecItems_ slow n format@Format{..} = format {
  formatItemDone = \ path item -> do
    let 
      location = itemLocation item
      duration = toMilliseconds (itemDuration item)
    when (duration /= 0) $ do
      liftIO $ modifyIORef slow (SlowItem{..}  :)
    formatItemDone path item
, formatRun = \ action -> formatRun action <* do
    xs <- take n . reverse . sortOn duration <$> readIORef slow
    unless (null xs) $ do
      putStrLn "\nSlow spec items:"
      mapM_ printSlowSpecItem xs
}

printSlowSpecItem :: SlowItem -> IO ()
printSlowSpecItem SlowItem{..} = do
  putStrLn $ "  " ++ maybe "" formatLocation location ++ joinPath path ++ " (" ++ show duration ++ "ms)"
