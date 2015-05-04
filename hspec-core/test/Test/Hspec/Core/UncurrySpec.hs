module Test.Hspec.Core.UncurrySpec (spec) where

import           Test.Hspec.Meta

import           Test.Hspec.Core.Uncurry

pass :: Expectation
pass = return ()

spec :: Spec
spec = do
  describe "uncurryT" $ do
    it "works for functions of arrity 0" $ do
      let _type = uncurryT :: r -> () -> r
      pass

    it "works for functions of arrity 1" $ do
      let _type = uncurryT :: (a -> r) -> (a, ()) -> r
      pass

    it "works for functions of arrity 2" $ do
      let _type = uncurryT :: (a -> b -> r) -> (b, (a, ())) -> r
      pass

    it "works for functions of arrity 3" $ do
      let _type = uncurryT :: (a -> b -> c -> r) -> (c, (b, (a, ()))) -> r
      pass

    it "works for functions of arrity 4" $ do
      let _type = uncurryT :: (a -> b -> c -> d -> r) -> (d, (c, (b, (a, ())))) -> r
      pass
