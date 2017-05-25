{-# LANGUAGE CPP #-}
module Test.Hspec.Core.QuickCheckUtil where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.Int
import           System.Random
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as QCP
import           Test.QuickCheck.Property hiding (Result(..))
import           Test.QuickCheck.Gen
import           Test.QuickCheck.IO ()


#if MIN_VERSION_QuickCheck(2,7,0)
import           Test.QuickCheck.Random
#endif

import           Test.Hspec.Core.Util

aroundProperty :: ((a -> IO ()) -> IO ()) -> (a -> Property) -> Property
#if MIN_VERSION_QuickCheck(2,7,0)
aroundProperty action p = MkProperty . MkGen $ \r n -> aroundProp action $ \a -> (unGen . unProperty $ p a) r n
#else
aroundProperty action p = MkGen $ \r n -> aroundProp action $ \a -> (unGen $ p a) r n
#endif

aroundProp :: ((a -> IO ()) -> IO ()) -> (a -> Prop) -> Prop
aroundProp action p = MkProp $ aroundRose action (\a -> unProp $ p a)

aroundRose :: ((a -> IO ()) -> IO ()) -> (a -> Rose QCP.Result) -> Rose QCP.Result
aroundRose action r = ioRose $ do
  ref <- newIORef (return QCP.succeeded)
  action $ \a -> reduceRose (r a) >>= writeIORef ref
  readIORef ref

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

formatNumbers :: Result -> String
formatNumbers r = "(after " ++ pluralize (numTests r) "test" ++ shrinks ++ ")"
  where
    shrinks
      | 0 < numShrinks r = " and " ++ pluralize (numShrinks r) "shrink"
      | otherwise = ""
