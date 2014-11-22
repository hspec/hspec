{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

class ToTuple a where
  type To a
  to :: a -> To a

instance ToTuple (a, (b, (c, ()))) where
  type To (a, (b, (c, ()))) = (a, b, c)
  to (a, (b, (c, ()))) = (a, b, c)

instance ToTuple (a, (b, ())) where
  type To (a, (b, ())) = (a, b)
  to (a, (b, ())) = (a, b)

instance ToTuple (a, ()) where
  type To (a, ()) = a
  to (a, ()) = a

instance ToTuple () where
  type To () = ()
  to () = ()

class FromTuple a where
  type From a
  from :: a -> From a

instance FromTuple (a, b, c) where
  type From (a, b, c) = (a, (b, (c, ())))
  from (a, b, c) = (a, (b, (c, ())))

instance FromTuple (a, b) where
  type From (a, b) = (a, (b, ()))
  from (a, b) = (a, (b, ()))

{-
instance FromTuple a where
  type From a = (a, ())
  from a = (a, ())
  -}
