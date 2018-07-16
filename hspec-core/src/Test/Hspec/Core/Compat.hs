{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Compat (
  getDefaultConcurrentJobs
, showType
, showFullType
, readMaybe
, lookupEnv
, module Data.IORef

, module Prelude
, module Control.Applicative
, module Control.Monad
, module Data.Foldable
, module Data.Traversable
, module Data.Monoid
, module Data.List

#if !MIN_VERSION_base(4,6,0)
, modifyIORef'
, atomicWriteIORef
#endif
, interruptible
) where

import           Control.Applicative
import           Control.Monad hiding (
    mapM
  , mapM_
  , forM
  , forM_
  , msum
  , sequence
  , sequence_
  )
import           Data.Foldable
import           Data.Traversable
import           Data.Monoid
import           Data.List (intercalate)

import           Prelude hiding (
    all
  , and
  , any
  , concat
  , concatMap
  , elem
  , foldl
  , foldl1
  , foldr
  , foldr1
  , mapM
  , mapM_
  , maximum
  , minimum
  , notElem
  , or
  , product
  , sequence
  , sequence_
  , sum
  )

import           Data.Typeable (Typeable, typeOf, typeRepTyCon)
import           Text.Read
import           Data.IORef
import           System.Environment

import           Data.Typeable (tyConModule, tyConName)
import           Control.Concurrent

#if MIN_VERSION_base(4,9,0)
import           Control.Exception (interruptible)
#else
import           GHC.IO
#endif

#if !MIN_VERSION_base(4,6,0)
import qualified Text.ParserCombinators.ReadP as P

-- |Strict version of 'modifyIORef'
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'

atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef ref a = do
    x <- atomicModifyIORef ref (\_ -> (a, ()))
    x `seq` return ()

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
  show t

showFullType :: Typeable a => a -> String
showFullType a = let t = typeRepTyCon (typeOf a) in
  tyConModule t ++ "." ++ tyConName t

getDefaultConcurrentJobs :: IO Int
getDefaultConcurrentJobs = getNumCapabilities

#if !MIN_VERSION_base(4,9,0)
interruptible :: IO a -> IO a
interruptible act = do
  st <- getMaskingState
  case st of
    Unmasked              -> act
    MaskedInterruptible   -> unsafeUnmask act
    MaskedUninterruptible -> act
#endif
