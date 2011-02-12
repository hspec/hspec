
module Test.Hspec.QuickCheck (
  property
) where

import Test.Hspec.Internal
import qualified Test.QuickCheck as QC


data QuickCheckProperty a = QuickCheckProperty a

property :: QC.Testable a => a -> QuickCheckProperty a
property = QuickCheckProperty

instance QC.Testable t => SpecVerifier (QuickCheckProperty t) where
  it n (QuickCheckProperty p) = do
    r <- QC.quickCheckResult p
    case r of
      QC.Success {} -> return (n, Success)
      _             -> return (n, Fail)
