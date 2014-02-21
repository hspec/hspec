module Test.Hspec.Core.QuickCheckUtil where

import           Control.Applicative
import           Control.Exception
import           Data.Int
import           Data.IORef
import           Test.QuickCheck hiding (Result(..))
import           Test.QuickCheck as QC
import           Test.QuickCheck.Property hiding (Result(..))
import qualified Test.QuickCheck.Property as QCP
import           Test.QuickCheck.IO ()
import           Test.QuickCheck.Random
import           System.Random

aroundProperty :: (IO () -> IO ()) -> Property -> Property
aroundProperty action (MkProperty p) = MkProperty $ MkProp . aroundRose action . unProp <$> p

aroundRose :: (IO () -> IO ()) -> Rose QCP.Result -> Rose QCP.Result
aroundRose action r = ioRose $ do
  ref <- newIORef (return QCP.succeeded)
  action (reduceRose r >>= writeIORef ref)
  readIORef ref

isUserInterrupt :: QC.Result -> Bool
isUserInterrupt r = case r of
  QC.Failure {theException = me} -> (me >>= fromException) == Just UserInterrupt
  _ -> False

newSeed :: IO Int
newSeed = fromIntegral <$> (fst . randomR (0, maxBound) <$> newQCGen :: IO Int32)
