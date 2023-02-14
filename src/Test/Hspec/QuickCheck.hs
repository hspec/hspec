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
, xprop
, fprop
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
-- @
-- `it` ".." $ `property` $
--   ..
-- @
prop :: (HasCallStack, Testable prop) => String -> prop -> Spec
prop s = it s . property


-- |
-- > xprop ".." $
-- >   ..
--
-- is a shortcut for
--
-- @
-- `xit` ".." $ `property` $
--   ..
-- @
xprop :: (HasCallStack, Testable prop) => String -> prop -> Spec
xprop s = xit s . property


-- |
-- > fprop ".." $
-- >   ..
--
-- is a shortcut for
--
-- @
-- `fit` ".." $ `property` $
--   ..
-- @
fprop :: (HasCallStack, Testable prop) => String -> prop -> Spec
fprop s = fit s . property
