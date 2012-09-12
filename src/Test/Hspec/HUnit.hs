{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Importing this module allows you to use an @HUnit@ `HU.Test` as an example
-- for a behavior.  You can use an explicit `HU.TestCase` data constructor or
-- use an `HU.Assertion`.  For an @Assertion@, any exception means the example
-- failed; otherwise, it's successfull.
--
-- NOTE: Any output from the example to @stdout@ is ignored.  If you need to
-- write out for debugging, you can write to @stderr@ or a file handle.
--
-- > import Test.Hspec.Monadic
-- > import Test.Hspec.HUnit ()
-- > import Test.HUnit
-- >
-- > main :: IO ()
-- > main = hspec $ do
-- >   describe "reverse" $ do
-- >     it "reverses a list" $ do
-- >       reverse [1, 2, 3] @?= [3, 2, 1]
-- >
-- >     it "gives the original list, if applied twice" $ TestCase $
-- >       (reverse . reverse) [1, 2, 3] @?= [1, 2, 3]
--
module Test.Hspec.HUnit () where

import           System.IO.Silently
import           Test.Hspec.Core
import qualified Test.HUnit as HU
import           Data.List (intersperse)

instance Example HU.Test where
  evaluateExample _ test = do
    (counts, fails) <- silence $ HU.runTestText HU.putTextToShowS test
    let r = if HU.errors counts + HU.failures counts == 0
             then Success
             else Fail (details $ fails "")
    return r
    where
      details :: String -> String
      details = concat . intersperse "\n" . tail . init . lines
