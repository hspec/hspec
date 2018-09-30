{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Test.Hspec.QuickCheck (
-- * Params
  modifyArgs
, modifyMaxSuccess
, modifyMaxDiscardRatio
, modifyMaxSize
, modifyMaxShrinks

-- * Shortcuts
, prop
) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.Hspec.Core.QuickCheck

-- |
-- > prop ".." $
-- >   ..
--
-- is a shortcut for
--
-- > it ".." $ property $
-- >   ..
prop :: (HasCallStack, Testable prop) => String -> prop -> Spec
prop s = it s . property
