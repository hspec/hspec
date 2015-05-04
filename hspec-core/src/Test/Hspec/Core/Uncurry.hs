{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Hspec.Core.Uncurry where

class UncurryT f where
  type Curry f
  uncurryT :: Curry f -> f

instance UncurryT (() -> r) where
  type Curry (() -> r) = r
  uncurryT f () = f

instance UncurryT ((a, ()) -> r) where
  type Curry ((a, ()) -> r) = a -> r
  uncurryT f (x, xs) = uncurryT f xs x

instance UncurryT ((b, (a, ())) -> r) where
  type Curry ((b, (a, ())) -> r) = a -> b -> r
  uncurryT f (x, xs) = uncurryT f xs x

instance UncurryT ((c, (b, (a, ()))) -> r) where
  type Curry ((c, (b, (a, ()))) -> r) = a -> b -> c -> r
  uncurryT f (x, xs) = uncurryT f xs x

instance UncurryT ((d, (c, (b, (a, ())))) -> r) where
  type Curry ((d, (c, (b, (a, ())))) -> r) = a -> b -> c -> d -> r
  uncurryT f (x, xs) = uncurryT f xs x

instance UncurryT ((e, (d, (c, (b, (a, ()))))) -> r) where
  type Curry ((e, (d, (c, (b, (a, ()))))) -> r) = a -> b -> c -> d -> e -> r
  uncurryT f (x, xs) = uncurryT f xs x
