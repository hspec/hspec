{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

type family To a where
  To (a, (b, (c, ()))) = (a, b, c)
  To (a, (b, ())) = (a, b)
  To (a, ()) = a
  To () = ()

class ToTuple a where
  to :: a -> To a

instance ToTuple (a, (b, (c, ()))) where
  to (a, (b, (c, ()))) = (a, b, c)

instance ToTuple (a, (b, ())) where
  to (a, (b, ())) = (a, b)

instance ToTuple (a, ()) where
  to (a, ()) = a

instance ToTuple () where
  to () = ()

type family IsTuple a :: Bool where
    IsTuple (a, b, c) = True
    IsTuple (a, b) = True
    IsTuple () = True
    IsTuple a = False

type family From a where
  From (a, b, c) = (a, (b, (c, ())))
  From (a, b) = (a, (b, ()))
  From () = ()
  From a = (a, ())

class (b ~ IsTuple a) => FromTuple a b where
  from :: a -> From a

instance FromTuple (a, b, c) True where
  from (a, b, c) = (a, (b, (c, ())))

instance FromTuple (a, b) True where
  from (a, b) = (a, (b, ()))

instance (From a ~ (a, ()), IsTuple a ~ False) => FromTuple a False where
  from a = (a, ())

instance FromTuple () True where
  from () = ()

modify :: (ToTuple a, FromTuple b (IsTuple b)) => (To a -> b) -> a -> From b
modify f = from . f . to

f :: (Int, String, Int) -> (Int, String)
f (a, b, c) = (a + c, b ++ "bar")

foo :: (Int, (String, (Int, ())))
foo = (23, ("foo", (42, ())))

bar :: (Int, (String, ()))
bar = modify f foo
