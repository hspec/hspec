module Test.Hspec.QuickCheck (
-- * Re-exports from QuickCheck
  property
-- * Shortcuts
, prop
) where

import           Test.QuickCheck
import           Test.Hspec.Monadic

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
