-- |
-- Importing this module allows you to use a QuickCheck `QC.Property` as an
-- example for a behavior.  Use `QC.property` to turn any `QC.Testable` into a
-- @Property@.
--
-- NOTE: Any output from the example to @stdout@ is ignored.  If you need to
-- write out for debugging, you can write to @stderr@ or a file handle.
--
-- > import Test.Hspec
-- > import Test.QuickCheck
-- >
-- > main :: IO ()
-- > main = hspec $ do
-- >   describe "reverse" $ do
-- >     it "gives the original list, if applied twice" $ property $
-- >       \xs -> (reverse . reverse) xs == (xs :: [Int])
module Test.Hspec.QuickCheck (
  QC.property
, prop
) where

import qualified Test.QuickCheck as QC

-- just for the prop shortcut
import qualified Test.Hspec.Monadic as DSL

-- | Monadic DSL shortcut, use this instead of `DSL.it`.
prop :: QC.Testable t => String -> t -> DSL.Spec
prop n p = DSL.it n (QC.property p)
