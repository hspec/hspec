{-# LANGUAGE CPP #-}
module Test.Hspec.Core.QuickCheckUtil where

import Data.IORef
import Test.QuickCheck hiding (Result(..))
import Test.QuickCheck as QC
import Test.QuickCheck.Property hiding (Result(..))
import qualified Test.QuickCheck.Property as QCP
import Test.QuickCheck.IO ()
import Control.Applicative

aroundProperty :: (IO () -> IO ()) -> Property -> Property
aroundProperty action p = MkProp . aroundRose action . unProp <$> p

aroundRose :: (IO () -> IO ()) -> Rose QCP.Result -> Rose QCP.Result
aroundRose action r = ioRose $ do
  ref <- newIORef (return QCP.succeeded)
  action (reduceRose r >>= writeIORef ref)
  readIORef ref

isUserInterrupt :: QC.Result -> Bool
isUserInterrupt r = case r of
#if MIN_VERSION_QuickCheck(2,6,0)
  QC.Failure {QC.interrupted = x} -> x
#else
  QC.Failure {QC.reason = "Exception: 'user interrupt'"} -> True
#endif
  _ -> False
