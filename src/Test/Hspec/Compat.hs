{-# LANGUAGE CPP #-}
module Test.Hspec.Compat where

import           Data.Typeable (Typeable, typeOf, typeRepTyCon)

#if MIN_VERSION_base(4,5,0)
import           Data.Typeable (tyConModule, tyConName)
#endif


showType :: Typeable a => a -> String
showType a = let t = typeRepTyCon (typeOf a) in
#if MIN_VERSION_base(4,5,0)
  tyConModule t ++ "." ++ tyConName t
#else
  show t
#endif
