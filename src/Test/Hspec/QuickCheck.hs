-- |
-- Stability: provisional
module Test.Hspec.QuickCheck (
-- * Params
  modifyMaxSuccess
, modifyMaxDiscardRatio
, modifyMaxSize
, modifyArgs
-- * Re-exports from QuickCheck
-- |
-- Previous versions of Hspec provided a distinct `property` combinator, but
-- it's now possible to use QuickCheck's `property` instead.  For backward
-- compatibility we now re-export QuickCheck's `property`, but it is advisable
-- to import it from "Test.QuickCheck" instead.
, property
-- * Shortcuts
, prop
) where

import           Test.QuickCheck
import           Test.Hspec
import           Test.Hspec.Core (QuickCheckArgs(..), modifyParams)

-- |
-- > prop ".." $
-- >   ..
--
-- is a shortcut for
--
-- > it ".." $ property $
-- >   ..
prop :: Testable prop => String -> prop -> Spec
prop s = it s . property

-- | Use a modified `maxSuccess` for given spec.
modifyMaxSuccess :: (Int -> Int) -> Spec -> Spec
modifyMaxSuccess = modifyArgs . modify
  where
    modify :: (Int -> Int) -> Args -> Args
    modify f args = args {maxSuccess = f (maxSuccess args)}

-- | Use a modified `maxDiscardRatio` for given spec.
modifyMaxDiscardRatio :: (Int -> Int) -> Spec -> Spec
modifyMaxDiscardRatio = modifyArgs . modify
  where
    modify :: (Int -> Int) -> Args -> Args
    modify f args = args {maxDiscardRatio = f (maxDiscardRatio args)}

-- | Use a modified `maxSize` for given spec.
modifyMaxSize :: (Int -> Int) -> Spec -> Spec
modifyMaxSize = modifyArgs . modify
  where
    modify :: (Int -> Int) -> Args -> Args
    modify f args = args {maxSize = f (maxSize args)}

modifyArgs :: (Args -> Args) -> Spec -> Spec
modifyArgs = modifyParams . modify
  where
    modify :: (Args -> Args) -> QuickCheckArgs -> QuickCheckArgs
    modify f p = QuickCheckArgs $ f $ quickCheckArgs p
