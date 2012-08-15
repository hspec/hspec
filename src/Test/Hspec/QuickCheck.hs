{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Importing this module allows you to use a QuickCheck `QC.Property` as an
-- example for a behavior.  Use `QC.property` to turn any `QC.Testable` into a
-- @Property@.
--
-- NOTE: Any output from the example to @stdout@ is ignored.  If you need to
-- write out for debugging, you can write to @stderr@ or a file handle.
--
-- > import Test.Hspec.Monadic
-- > import Test.Hspec.QuickCheck
-- >
-- > main :: IO ()
-- > main = hspec $ do
-- >   describe "reverse" $ do
-- >     it "gives the original list, if applied twice" $ property $
-- >       \xs -> (reverse . reverse) xs == (xs :: [Int])
--
module Test.Hspec.QuickCheck (
  QC.property
, prop
, QuickCheckExample
-- ** QuickCheck customizations
, quickCheckExample
) where

import           System.IO.Silently
import           Test.Hspec.Core
import           Test.Hspec.Config (Config(..))
import qualified Test.QuickCheck as QC

-- just for the prop shortcut
import qualified Test.Hspec.Monadic as DSL

-- | Monadic DSL shortcut, use this instead of `DSL.it`.
prop :: QC.Testable t => String -> t -> DSL.Spec
prop n p = DSL.it n (QC.property p)

instance Example QC.Property where
  evaluateExample c p = do
    r <- silence $ QC.quickCheckWithResult (configQuickCheckArgs c) p
    return $
      case r of
        QC.Success {}               -> Success
        f@(QC.Failure {})           -> Fail (QC.output f)
        QC.GaveUp {QC.numTests = n} -> Fail ("Gave up after " ++ quantify n "test" )
        QC.NoExpectedFailure {}     -> Fail ("No expected failure")

-- | A custom QuickCheck test to be run by hspec.
data QuickCheckExample = QuickCheckExample QC.Args QC.Property

-- | Runs a QuickCheck property with custom settings. Overrides `configQuickCheckArgs`.
-- Example:
--
-- > it "passes 1000 checks" $
-- >   quickCheckExample stdArgs{ maxSuccess = 1000 } myprop
quickCheckExample :: QC.Args -> QC.Property -> QuickCheckExample
quickCheckExample = QuickCheckExample

instance Example QuickCheckExample where
  evaluateExample c (QuickCheckExample args p) =
    evaluateExample c {configQuickCheckArgs = args} p
