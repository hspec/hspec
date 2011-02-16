{-# OPTIONS -XFlexibleInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Test.Hspec.HUnit
-- Copyright   :  (c) Trystan Spangler 2011
-- License     :  modified BSD
--
-- Maintainer  : trystan.s@comcast.net
-- Stability   : experimental
-- Portability : portable
--
-- |
--
-----------------------------------------------------------------------------

module Test.Hspec.HUnit (
) where

import Test.Hspec.Internal
import qualified Test.HUnit as HU

-- | Use an hUnit test case as verification of a spec.
--
-- > describe "cutTheDeck" [
-- >   it "puts the first half of a list after the last half"
-- >      (assertEqual "cut the deck" [3,4,1,2] (cutTheDeck [1,2,3,4])),
-- >
-- >   it "restores an even sized list when cut twice"
-- >      (assertEqual "cut the deck twice" [3,4,1,2] (cutTheDeck (cutTheDeck [1,2,3,4]))),
-- >   ]
--
instance SpecVerifier (IO ()) where
  it n t = it n (HU.TestCase t)

instance SpecVerifier HU.Test where
  it n t = do
    -- runTestText :: PutText st -> Test -> IO (Counts, st)
    (counts, _) <- HU.runTestText (HU.PutText (\ _ _ st -> return st) ()) t
    if HU.errors counts + HU.failures counts == 0
      then return (n, Success)
      else return (n, Fail)

