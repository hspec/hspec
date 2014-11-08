module Test.Hspec.QuickCheck (
  module Test.Hspec.Core.QuickCheck
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
prop :: Testable prop => String -> prop -> Spec
prop s = it s . property
