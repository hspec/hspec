{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Hspec.Core.Uncurry where

class UncurryT a where
  type F a
  uncurryT :: F a -> a

instance UncurryT (() -> r) where
  type F (() -> r) = r
  uncurryT f () = f

instance UncurryT ((a, ()) -> r) where
  type F ((a, ()) -> r) = a -> r
  uncurryT f (x, xs) = uncurryT f xs x

instance UncurryT ((b, (a, ())) -> r) where
  type F ((b, (a, ())) -> r) = a -> b -> r
  uncurryT f (x, xs) = uncurryT f xs x

instance UncurryT ((c, (b, (a, ()))) -> r) where
  type F ((c, (b, (a, ()))) -> r) = a -> b -> c -> r
  uncurryT f (x, xs) = uncurryT f xs x

instance UncurryT ((d, (c, (b, (a, ())))) -> r) where
  type F ((d, (c, (b, (a, ())))) -> r) = a -> b -> c -> d -> r
  uncurryT f (x, xs) = uncurryT f xs x
