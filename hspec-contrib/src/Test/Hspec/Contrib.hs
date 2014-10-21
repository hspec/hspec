{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Test.Hspec.Contrib (
  retryWith
) where

import           Test.Hspec.Core.Type

data Retry a = Retry Int a

instance Example a => Example (Retry a) where
  evaluateExample (Retry n example) a b c = do
    v <- evaluateExample example a b c
    case v of
      Success -> return v
      _ | n > 1 -> evaluateExample (Retry (pred n) example) a b c
      _ -> return v

retryWith :: Int -> a -> Retry a
retryWith = Retry
