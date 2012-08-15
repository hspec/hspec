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
, QuickCheckExample
-- ** QuickCheck customizations
, quickCheckExample
) where

import           Test.QuickCheck
import           Test.Hspec.Core (Example(..), Params(..))
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

-- | A custom QuickCheck test to be run by hspec.
data QuickCheckExample = QuickCheckExample Args Property

-- | Runs a QuickCheck property with custom settings. Overrides `paramsQuickCheckArgs`.
-- Example:
--
-- > it "passes 1000 checks" $
-- >   quickCheckExample stdArgs{ maxSuccess = 1000 } myprop
quickCheckExample :: Args -> Property -> QuickCheckExample
quickCheckExample = QuickCheckExample

instance Example QuickCheckExample where
  evaluateExample c (QuickCheckExample args p) =
    evaluateExample c {paramsQuickCheckArgs = args} p
