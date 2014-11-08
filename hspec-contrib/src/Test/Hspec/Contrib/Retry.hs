{-# LANGUAGE TypeFamilies #-}
module Test.Hspec.Contrib.Retry (retryWith) where

import           Test.Hspec.Core

data Retry a = Retry Int a

instance Example a => Example (Retry a) where
  type Arg (Retry a) = Arg a
  evaluateExample (Retry n example) a b c = do
    v <- evaluateExample example a b c
    case v of
      Success -> return v
      _ | n > 1 -> evaluateExample (Retry (pred n) example) a b c
      _ -> return v

-- | Retry evaluating example that may be failed until success.
retryWith :: Int
          -- ^ number of retries, when this number is 1, just evaluate example and finish.
          -> a
          -- ^ retried example
          -> Retry a
          -- ^ Retry is instance of Example.
retryWith = Retry
