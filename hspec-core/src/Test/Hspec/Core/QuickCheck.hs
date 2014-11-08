-- |
-- Stability: provisional
module Test.Hspec.Core.QuickCheck (
  modifyMaxSuccess
, modifyMaxDiscardRatio
, modifyMaxSize

) where

import           Test.QuickCheck
import           Test.Hspec.Core.Spec
import           Test.Hspec.Core (Params(..), modifyParams)

-- | Use a modified `maxSuccess` for given spec.
modifyMaxSuccess :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxSuccess = modifyArgs . modify
  where
    modify :: (Int -> Int) -> Args -> Args
    modify f args = args {maxSuccess = f (maxSuccess args)}

-- | Use a modified `maxDiscardRatio` for given spec.
modifyMaxDiscardRatio :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxDiscardRatio = modifyArgs . modify
  where
    modify :: (Int -> Int) -> Args -> Args
    modify f args = args {maxDiscardRatio = f (maxDiscardRatio args)}

-- | Use a modified `maxSize` for given spec.
modifyMaxSize :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxSize = modifyArgs . modify
  where
    modify :: (Int -> Int) -> Args -> Args
    modify f args = args {maxSize = f (maxSize args)}

modifyArgs :: (Args -> Args) -> SpecWith a -> SpecWith a
modifyArgs = modifyParams . modify
  where
    modify :: (Args -> Args) -> Params -> Params
    modify f p = p {paramsQuickCheckArgs = f (paramsQuickCheckArgs p)}
