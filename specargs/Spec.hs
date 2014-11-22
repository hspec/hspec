{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
import           Test.Hspec hiding (before, around, around_)
import           Test.Hspec.Core.Spec

newtype Foo = Foo {unFoo :: IO ()}

instance Example Foo where
  type Arg Foo = ()
  evaluateExample (Foo action) _ around_ _ = do
    around_ (\() -> action)
    return Success

instance Example (a -> Foo) where
  type Arg (a -> Foo) = (a, ())
  evaluateExample action _ around_ _ = do
    around_ (\(a, ()) -> unFoo (action a))
    return Success

instance Example (a -> b -> Foo) where
  type Arg (a -> b -> Foo) = (a, (b, ()))
  evaluateExample action _ around_ _ = do
    around_ (\(a, (b, ())) -> unFoo (action a b))
    return Success

instance Example (a -> b -> c -> Foo) where
  type Arg (a -> b -> c -> Foo) = (a, (b, (c, ())))
  evaluateExample action _ around_ _ = do
    around_ (\(a, (b, (c, ()))) -> unFoo (action a b c))
    return Success

before :: IO a -> SpecWith (a, b) -> SpecWith b
before action = around (action >>=)

around :: (ActionWith a -> IO ()) -> SpecWith (a, b) -> SpecWith b
around f = mapAround foo
  where
    foo g h = f $ \a -> g $ \b -> h (a, b)

mapAround :: ((ActionWith b -> IO ()) -> ActionWith a -> IO ()) -> SpecWith a -> SpecWith b
mapAround f = mapSpecItem (untangle f) $ \i@Item{itemExample = e} -> i{itemExample = (. f) . e}

untangle  :: ((ActionWith b -> IO ()) -> ActionWith a -> IO ()) -> ActionWith a -> ActionWith b
untangle f g = \b -> f ($ b) g

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  before (return "foo") . before (return 23 :: IO Int) . before (return 42.0 :: IO Double) $ do
    describe "Foo" $ do
      it "can take up to three arguments" $ \a b c -> Foo $ do
        print (a, b, c)
