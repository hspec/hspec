{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module ActionTF where

import Prelude hiding (curry, uncurry)

type Example0         = () -> IO ()
type Example1 a       = (a, ()) -> IO ()
type Example2 a b     = (a, (b, ())) -> IO ()
type Example3 a b c   = (a, (b, (c, ()))) -> IO ()

type Example0_        = IO ()
type Example1_ a      = a -> IO ()
type Example2_ a b    = a -> b -> IO ()
type Example3_ a b c  = a -> b -> c -> IO ()

curry0 :: Example0 -> Example0_
curry0 = curry

curry1 :: Example1 a -> Example1_ a
curry1 = curry

curry2 :: Example2 a b -> Example2_ a b
curry2 = curry

curry3 :: Example3 a b c -> Example3_ a b c
curry3 = curry

f :: (a, (b, (c, ()))) -> IO ()
f = undefined

foobar :: a -> b -> c -> IO ()
foobar = curry f

class Curry a where
  type Curried a
  curry :: (a -> IO ()) -> Curried a

instance Curry b => Curry (a, b) where
  type Curried (a, b) = a -> Curried b
  curry action a = curry (\b -> action (a, b))

instance Curry () where
  type Curried () = IO ()
  curry = ($ ())

class Uncurry a where
  type Uncurried a
  uncurry :: a -> (Uncurried a -> IO ())

instance Uncurry b => Uncurry (a -> b) where
  type Uncurried (a -> b) = (a, Uncurried b)
  uncurry action (a, b) = uncurry (action a) b

instance Uncurry (IO ()) where
  type Uncurried (IO ()) = ()
  uncurry action = \() -> action

uncurry0 :: Example0_ -> Example0
uncurry0 = uncurry

uncurry1 :: Example1_ a -> Example1 a
uncurry1 = uncurry

uncurry2 :: Example2_ a b -> Example2 a b
uncurry2 = uncurry

uncurry3 :: Example3_ a b c -> Example3 a b c
uncurry3 = uncurry

baz :: (((a, (b, (c, ()))) -> IO ()) -> IO ()) -> (a -> b -> c -> IO ()) -> IO ()
baz = (. uncurry)
