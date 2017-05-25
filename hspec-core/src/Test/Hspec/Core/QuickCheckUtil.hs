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
import           Test.QuickCheck.Random

import           Test.Hspec.Core.Util

aroundProperty :: ((a -> IO ()) -> IO ()) -> (a -> Property) -> Property
aroundProperty action p = MkProperty . MkGen $ \r n -> aroundProp action $ \a -> (unGen . unProperty $ p a) r n

aroundProp :: ((a -> IO ()) -> IO ()) -> (a -> Prop) -> Prop
aroundProp action p = MkProp $ aroundRose action (\a -> unProp $ p a)

aroundRose :: ((a -> IO ()) -> IO ()) -> (a -> Rose QCP.Result) -> Rose QCP.Result
aroundRose action r = ioRose $ do
  ref <- newIORef (return QCP.succeeded)
  action $ \a -> reduceRose (r a) >>= writeIORef ref
  readIORef ref

newSeed :: IO Int
newSeed = fst . randomR (0, fromIntegral (maxBound :: Int32)) <$>
  newQCGen

mkGen :: Int -> QCGen
mkGen = mkQCGen

formatNumbers :: Result -> String
formatNumbers r = "(after " ++ pluralize (numTests r) "test" ++ shrinks ++ ")"
  where
    shrinks
      | 0 < numShrinks r = " and " ++ pluralize (numShrinks r) "shrink"
      | otherwise = ""
