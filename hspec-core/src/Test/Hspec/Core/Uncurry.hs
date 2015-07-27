{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Hspec.Core.Uncurry where

class UncurryT f where
  type Curry f
  type ArgC f
  type T f
  uncurryT :: Curry f -> f

instance UncurryT (() -> r) where
  type Curry (() -> r) = r
  type ArgC (() -> r) = ()
  type T (() -> r) = r
  uncurryT f () = f

instance UncurryT ((a, ()) -> r) where
  type Curry ((a, ()) -> r) = a -> r
  type ArgC ((a, ()) -> r) = (a, ())
  type T ((a, ()) -> r) = r
  uncurryT f (x, xs) = uncurryT f xs x

instance UncurryT ((b, (a, ())) -> r) where
  type Curry ((b, (a, ())) -> r) = a -> b -> r
  type ArgC ((b, (a, ())) -> r) = (b, (a, ()))
  type T ((b, (a, ())) -> r) = r
  uncurryT f (x, xs) = uncurryT f xs x

instance UncurryT ((c, (b, (a, ()))) -> r) where
  type Curry ((c, (b, (a, ()))) -> r) = a -> b -> c -> r
  type ArgC ((c, (b, (a, ()))) -> r) = (c, (b, (a, ())))
  type T ((c, (b, (a, ()))) -> r) = r
  uncurryT f (x, xs) = uncurryT f xs x

instance UncurryT ((d, (c, (b, (a, ())))) -> r) where
  type Curry ((d, (c, (b, (a, ())))) -> r) = a -> b -> c -> d -> r
  type ArgC ((d, (c, (b, (a, ())))) -> r) = (d, (c, (b, (a, ()))))
  type T ((d, (c, (b, (a, ())))) -> r) = r
  uncurryT f (x, xs) = uncurryT f xs x

instance UncurryT ((e, (d, (c, (b, (a, ()))))) -> r) where
  type Curry ((e, (d, (c, (b, (a, ()))))) -> r) = a -> b -> c -> d -> e -> r
  type ArgC ((e, (d, (c, (b, (a, ()))))) -> r) = (e, (d, (c, (b, (a, ())))))
  type T ((e, (d, (c, (b, (a, ()))))) -> r) = r
  uncurryT f (x, xs) = uncurryT f xs x
