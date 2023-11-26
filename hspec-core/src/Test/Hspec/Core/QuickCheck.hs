-- | Stability: provisional
module Test.Hspec.Core.QuickCheck (
  modifyArgs
, modifyOptions
, modifyMaxSuccess
, modifyMaxDiscardRatio
, modifyMaxSize
, modifyMaxShrinks
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Test.QuickCheck
import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Spec

-- | Use a modified `maxSuccess` for given spec.
modifyMaxSuccess :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxSuccess = modifyOptions . modify
  where
    modify :: (Int -> Int) -> QuickCheckOptions -> QuickCheckOptions
    modify f args = args {qMaxSuccess = Just $ f (qqMaxSuccess args)}

-- | Use a modified `maxDiscardRatio` for given spec.
modifyMaxDiscardRatio :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxDiscardRatio = modifyOptions . modify
  where
    modify :: (Int -> Int) -> QuickCheckOptions -> QuickCheckOptions
    modify f args = args {qMaxDiscardRatio = Just $ f (qqMaxDiscardRatio args)}

-- | Use a modified `maxSize` for given spec.
modifyMaxSize :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxSize = modifyOptions . modify
  where
    modify :: (Int -> Int) -> QuickCheckOptions -> QuickCheckOptions
    modify f args = args {qMaxSize = Just $ f (qqMaxSize args)}

-- | Use a modified `maxShrinks` for given spec.
modifyMaxShrinks :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxShrinks = modifyOptions . modify
  where
    modify :: (Int -> Int) -> QuickCheckOptions -> QuickCheckOptions
    modify f args = args {qMaxShrinks = Just $ f (qqMaxShrinks args)}

-- | Use modified `QuickCheckOptions` for given spec.
modifyOptions :: (QuickCheckOptions -> QuickCheckOptions) -> SpecWith a -> SpecWith a
modifyOptions = modifyParams

-- | Use modified `Args` for given spec.
modifyArgs :: (Args -> Args) -> SpecWith a -> SpecWith a
modifyArgs f = modifyOptions $ \ options -> options { qModifyArgs = f . qModifyArgs options }
