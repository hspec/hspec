{-# LANGUAGE CPP #-}
module Test.Hspec.Core.QuickCheckUtil where

import Data.IORef
import Test.QuickCheck hiding (Result(..))
import Test.QuickCheck as QC
import Test.QuickCheck.Property hiding (Result(..))
import Test.QuickCheck.Gen
import qualified Test.QuickCheck.Property as QCP
import Test.QuickCheck.IO ()

aroundProperty :: ((a -> IO ()) -> IO ()) -> (a -> Property) -> Property
aroundProperty action p = MkGen $ \r n -> aroundProp action $ \a -> (unGen $ p a) r n

aroundProp :: ((a -> IO ()) -> IO ()) -> (a -> Prop) -> Prop
aroundProp action p = MkProp $ aroundRose action (\a -> unProp $ p a)

aroundRose :: ((a -> IO ()) -> IO ()) -> (a -> Rose QCP.Result) -> Rose QCP.Result
aroundRose action r = ioRose $ do
  ref <- newIORef (return QCP.succeeded)
  action $ \a -> reduceRose (r a) >>= writeIORef ref
  readIORef ref

isUserInterrupt :: QC.Result -> Bool
isUserInterrupt r = case r of
#if MIN_VERSION_QuickCheck(2,6,0)
  QC.Failure {QC.interrupted = x} -> x
#else
  QC.Failure {QC.reason = "Exception: 'user interrupt'"} -> True
#endif
  _ -> False
