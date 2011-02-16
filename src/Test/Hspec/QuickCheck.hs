-----------------------------------------------------------------------------
--
-- Module      :  Test.Hspec.QuickCheck
-- Copyright   :  (c) Trystan Spangler 2011
-- License     :  modified BSD
--
-- Maintainer  : trystan.s@comcast.net
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------


-- | Importing this module allows you to use a QuickCheck property as an example
-- for a requirement. Use the 'property' function to indicate a QuickCkeck property.
--
-- > describe "cutTheDeck" [
-- >   it "puts the first half of a list after the last half"
-- >      (property $ \ xs -> let top = take (length xs `div` 2) xs
-- >                              bot = drop (length xs `div` 2) xs
-- >                          in cutTheDeck xs == bot ++ top),
-- >
-- >   it "restores an even sized list when cut twice"
-- >      (property $ \ xs -> even (length xs) ==> cutTheDeck (cutTheDeck xs) == xs)
-- >   ]
--
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
      _             -> return (n, Fail "")
