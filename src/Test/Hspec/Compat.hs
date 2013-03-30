{-# LANGUAGE CPP #-}
module Test.Hspec.Compat (
  showType
, showFullType
, isUserInterrupt
, readMaybe
) where

import           Data.Typeable (Typeable, typeOf, typeRepTyCon)
import qualified Test.QuickCheck as QC
import           Text.Read

#if MIN_VERSION_base(4,4,0)
import           Data.Typeable.Internal (tyConModule, tyConName)
#endif

#if !MIN_VERSION_base(4,6,0)
import qualified Text.ParserCombinators.ReadP as P
import Prelude

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
#if MIN_VERSION_QuickCheck(2,6,0)
  QC.Failure {QC.interrupted = x} -> x
#else
  QC.Failure {QC.reason = "Exception: 'user interrupt'"} -> True
#endif
  _ -> False
