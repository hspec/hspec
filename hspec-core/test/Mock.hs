module Mock where

import           Control.Applicative
import           Data.IORef

newtype Mock = Mock (IORef Int)

newMock :: IO Mock
newMock = Mock <$> newIORef 0

mockAction :: Mock -> IO ()
mockAction (Mock ref) = modifyIORef ref succ

mockCounter  :: Mock -> IO Int
mockCounter  (Mock ref) = readIORef ref
