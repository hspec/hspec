{-# LANGUAGE CPP #-}
module Test.Hspec.Compat where

import           Data.Typeable (Typeable, typeOf, typeRepTyCon)
import qualified Test.QuickCheck as QC

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

isUserInterrupt :: QC.Result -> Bool
isUserInterrupt r = case r of
#if MIN_VERSION_base(2,6,0)
  QC.Failure {QC.interrupted = r} -> r
#else
  QC.Failure {QC.reason = "Exception: 'user interrupt'"} -> True
#endif
  _ -> False
