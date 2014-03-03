{-# LANGUAGE CPP #-}
module Test.Hspec.Compat (
  showType
, showFullType
, lookupEnv
, module Data.IORef
#if !MIN_VERSION_base(4,6,0)
, modifyIORef'
#endif
) where

import           Data.Typeable (Typeable, typeOf, typeRepTyCon)
import           Data.IORef
import           System.Environment

#if MIN_VERSION_base(4,4,0)
import           Data.Typeable.Internal (tyConModule, tyConName)
#endif

#if !MIN_VERSION_base(4,6,0)
-- |Strict version of 'modifyIORef'
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'

-- | Return the value of the environment variable @var@, or @Nothing@ if
-- there is no such value.
--
-- For POSIX users, this is equivalent to 'System.Posix.Env.getEnv'.
lookupEnv :: String -> IO (Maybe String)
lookupEnv k = lookup k `fmap` getEnvironment
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
