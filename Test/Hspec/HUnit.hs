{-# OPTIONS -XFlexibleInstances -fno-warn-orphans #-}

-- | Importing this module allows you to use an @HUnit@ test case as an example
-- for a behavior. You can use an explicit @TestCase@ data constructor or
-- use an @IO()@ action. For an @IO()@ action, any exception means the example
-- failed; otherwise, it's successfull.
--
-- > describe "cutTheDeck" [
-- >   it "puts the first half of a list after the last half"
-- >      (TestCase $ assertEqual "cut the deck" [3,4,1,2] (cutTheDeck [1,2,3,4])),
-- >
-- >   it "restores an even sized list when cut twice"
-- >      (assertEqual "cut the deck twice" [3,4,1,2] (cutTheDeck (cutTheDeck [1,2,3,4]))),
-- >   ]
-- >
module Test.Hspec.HUnit (
) where

import System.IO.Silently
import Test.Hspec.Core
import qualified Test.HUnit as HU
import Data.List (intersperse)

instance Example (IO ()) where
  evaluateExample example = evaluateExample (HU.TestCase example)

instance Example HU.Test where
  evaluateExample example = do
    (counts, fails) <- silence $ HU.runTestText HU.putTextToShowS example
    let result = if HU.errors counts + HU.failures counts == 0
             then Success
             else Fail (details $ fails "")
    return result

details :: String -> String
details = concat . intersperse "\n" . tail . init . lines
