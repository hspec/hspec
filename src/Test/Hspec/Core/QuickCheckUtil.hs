{-# LANGUAGE CPP #-}
module Test.Hspec.Core.QuickCheckUtil where

import           Control.Applicative
import           Data.Int
import           Data.IORef
import           Test.QuickCheck hiding (Result(..))
import           Test.QuickCheck as QC
import           Test.QuickCheck.Property hiding (Result(..))
import qualified Test.QuickCheck.Property as QCP
import           Test.QuickCheck.IO ()


#if MIN_VERSION_QuickCheck(2,7,0)
import           Test.QuickCheck.Random
#endif

import           System.Random

import           Test.Hspec.Util

aroundProperty :: (IO () -> IO ()) -> Property -> Property
#if MIN_VERSION_QuickCheck(2,7,0)
aroundProperty action (MkProperty p) = MkProperty $ MkProp . aroundRose action . unProp <$> p
#else
aroundProperty action p = MkProp . aroundRose action . unProp <$> p
#endif

aroundRose :: (IO () -> IO ()) -> Rose QCP.Result -> Rose QCP.Result
aroundRose action r = ioRose $ do
  ref <- newIORef (return QCP.succeeded)
  action (reduceRose r >>= writeIORef ref)
  readIORef ref

formatNumbers :: Result -> String
formatNumbers r = "(after " ++ pluralize (numTests r) "test" ++ shrinks ++ ")"
  where
    shrinks
      | 0 < numShrinks r = " and " ++ pluralize (numShrinks r) "shrink"
      | otherwise = ""

newSeed :: IO Int
newSeed = fst . randomR (0, fromIntegral (maxBound :: Int32)) <$>
#if MIN_VERSION_QuickCheck(2,7,0)
  newQCGen
#else
  newStdGen
#endif

#if MIN_VERSION_QuickCheck(2,7,0)
mkGen :: Int -> QCGen
mkGen = mkQCGen
#else
mkGen :: Int -> StdGen
mkGen = mkStdGen
#endif
