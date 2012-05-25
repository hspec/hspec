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
-- > main = hspecX $ do
-- >   describe "reverse" $ do
-- >     it "gives the original list, if applied twice" $ property $
-- >       \xs -> (reverse . reverse) xs == (xs :: [Int])
--
module Test.Hspec.QuickCheck (
  QC.property
, prop
) where

import           System.IO.Silently
import           Test.Hspec.Core
import qualified Test.QuickCheck as QC

-- just for the prop shortcut
import qualified Test.Hspec.Monadic as DSL

-- | Monadic DSL shortcut, use this instead of `DSL.it`.
prop :: QC.Testable t => String -> t -> DSL.Spec
prop n p = DSL.it n (QC.property p)

instance Example QC.Property where
  evaluateExample p = do
    r <- silence $ QC.quickCheckResult p
    let r' = case r of
              QC.Success {}           -> Success
              f@(QC.Failure {})       -> Fail (QC.output f)
              g@(QC.GaveUp {})        -> Fail ("Gave up after " ++ quantify (QC.numTests g) "test" )
              QC.NoExpectedFailure {} -> Fail ("No expected failure")
    return r'
