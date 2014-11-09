-- | Stability: unstable
module Test.Hspec.Core {-# DEPRECATED "use \"Test.Hspec.Core.Spec\" instead" #-} (
  module Test.Hspec.Core.Spec
-- * Deprecated functions
, describe
, it
) where

import           Test.Hspec.Core.Spec hiding (describe, it)


{-# DEPRECATED describe "use `specGroup` instead" #-}
describe :: String -> [SpecTree a] -> SpecTree a
describe = specGroup

{-# DEPRECATED it "use `specItem` instead" #-}
it :: Example a => String -> a -> SpecTree (Arg a)
it = specItem
