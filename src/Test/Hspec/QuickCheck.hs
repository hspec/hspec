{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,8,1)
#define HAS_SOURCE_LOCATIONS
{-# LANGUAGE ImplicitParams #-}
#endif
module Test.Hspec.QuickCheck (
-- * Params
  modifyMaxSuccess
, modifyMaxDiscardRatio
, modifyMaxSize

-- * Shortcuts
, prop
) where

#ifdef HAS_SOURCE_LOCATIONS
import           GHC.Stack
#endif

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
#ifdef HAS_SOURCE_LOCATIONS
prop :: (?loc :: CallStack, Testable prop) => String -> prop -> Spec
#else
prop :: (Testable prop) => String -> prop -> Spec
#endif
prop s = it s . property
