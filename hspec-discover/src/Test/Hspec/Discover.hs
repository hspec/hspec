{-# OPTIONS_HADDOCK prune #-}
-- |
-- Say you have a driver module for your test suite.
--
-- > module Main where
-- >
-- > import Test.Hspec.Monadic
-- >
-- > import qualified FooSpec
-- > import qualified Foo.BarSpec
-- > import qualified BazSpec
-- >
-- > main :: IO ()
-- > main = hspecX $ do
-- >   describe "Foo"     FooSpec.spec
-- >   describe "Foo.Bar" Foo.BarSpec.spec
-- >   describe "Baz"     BazSpec.spec
--
-- Then you can replace it with the following.
--
-- > {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
--
-- All files with a name that ends in @Spec.hs@ are include in the generated
-- test suite.  And it is assumed, that they export a @spec@ of type
-- `Test.Hspec.Monadic.Specs`.
--
-- Full documentation is here: <https://github.com/sol/hspec-discover#readme>
module Test.Hspec.Discover (hspec, describe) where

import           Test.Hspec.Monadic (Specs)
import qualified Test.Hspec.Monadic as Hspec

hspec :: Specs -> IO ()
hspec = Hspec.hspecX

describe :: String -> Specs -> Specs
describe = Hspec.describe
