-- |
-- Stability: provisional
module Test.Hspec.QuickCheck (
-- * Re-exports from QuickCheck
-- |
-- Previous versions of Hspec provided a distinct `property` combinator, but
-- it's now possible to use QuickCheck's `property` instead.  For backward
-- compatibility we now re-export QuickCheck's `property`, but it is advisable
-- to import it from "Test.QuickCheck" instead.
  property
-- * Shortcuts
, prop
) where

import           Test.QuickCheck
import           Test.Hspec

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
