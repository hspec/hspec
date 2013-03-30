module Test.Hspec.Timer where

import           Data.IORef
import           Data.Time.Clock.POSIX

newTimer :: POSIXTime -> IO (IO Bool)
newTimer delay = do
  ref <- getPOSIXTime >>= newIORef
  return $ do
    t0 <- readIORef ref
    t1 <- getPOSIXTime
    if delay < t1 - t0
      then writeIORef ref t1 >> return True
      else return False
