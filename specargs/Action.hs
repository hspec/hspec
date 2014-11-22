{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, InstanceSigs #-}

import Prelude hiding (curry, uncurry)

type Example0         = () -> IO ()
type Example1 a       = (a, ()) -> IO ()
type Example2 a b     = (a, (b, ())) -> IO ()
type Example3 a b c   = (a, (b, (c, ()))) -> IO ()

type Example0_        = IO ()
type Example1_ a      = a -> IO ()
type Example2_ a b    = a -> b -> IO ()
type Example3_ a b c  = a -> b -> c -> IO ()


f :: (a, (b, (c, ()))) -> IO ()
f = undefined

foobar :: a -> b -> c -> IO ()
foobar = curry f

class Curry a b | a -> b, b -> a where
  curry :: (a -> IO ()) -> b

instance Curry () (IO ()) where
  curry = ($ ())

instance Curry b c => Curry (a, b) (a -> c) where
  curry :: ((a, b) -> IO ()) -> a -> c
  curry action a = curry (\b -> action (a, b))

curry0 :: Example0 -> Example0_
curry0 = curry

curry1 :: Example1 a -> Example1_ a
curry1 = curry

curry2 :: Example2 a b -> Example2_ a b
curry2 = curry

curry3 :: Example3 a b c -> Example3_ a b c
curry3 = curry
