
-- | Importing this module allows you to use a QuickCheck property as an example
-- for a behavior. Use the 'property' function to indicate a QuickCkeck property.
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
  it description (QuickCheckProperty prop) = do
    r <- QC.quickCheckResult prop
    case r of
      QC.Success {}           -> return (description, Success)
      f@(QC.Failure {})       -> return (description, Fail (QC.output f))
      g@(QC.GaveUp {})        -> return (description, Fail ("Gave up after " ++ quantify (QC.numTests g) "test" ))
      QC.NoExpectedFailure {} -> return (description, Fail ("No expected failure"))
