{-# LANGUAGE CPP #-}
module Test.Hspec.Compat where

import           Data.Typeable (Typeable, typeOf, typeRepTyCon)

#if MIN_VERSION_base(4,4,0)
import           Data.Typeable.Internal (tyConModule, tyConName)
#endif

showType :: Typeable a => a -> String
showType a = let t = typeRepTyCon (typeOf a) in
#if MIN_VERSION_base(4,4,0)
  show t
#else
  (reverse . takeWhile (/= '.') . reverse . show) t
#endif


showFullType :: Typeable a => a -> String
showFullType a = let t = typeRepTyCon (typeOf a) in
#if MIN_VERSION_base(4,4,0)
  tyConModule t ++ "." ++ tyConName t
#else
  show t
#endif
