module Formatter (count) where

import           Data.IORef
import           Control.Monad.IO.Class
import           Test.Hspec.Core.Formatters.V1

count :: IO Formatter
count = do
  ref <- newIORef (0 :: Int)
  return silent {
      exampleSucceeded = \ _ _ -> liftIO (modifyIORef ref succ)
    , footerFormatter = liftIO (readIORef ref) >>= writeLine . show
    }
