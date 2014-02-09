{-# LANGUAGE CPP #-}
module Test.Hspec.Compat (
  showType
, showFullType
, readMaybe
, lookupEnv
, module Data.IORef
#if !MIN_VERSION_base(4,6,0)
, modifyIORef'
#endif
) where

import           Data.Typeable (Typeable, typeOf, typeRepTyCon)
import           Text.Read
import           Data.IORef
import           System.Environment

#if MIN_VERSION_base(4,4,0)
import           Data.Typeable.Internal (tyConModule, tyConName)
#endif

#if !MIN_VERSION_base(4,6,0)
import qualified Text.ParserCombinators.ReadP as P
#endif

#if !MIN_VERSION_base(4,6,0)
-- |Strict version of 'modifyIORef'
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
-- A 'Left' value indicates a parse error.
readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift P.skipSpaces
       return x

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case readEither s of
                Left _  -> Nothing
                Right a -> Just a

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
