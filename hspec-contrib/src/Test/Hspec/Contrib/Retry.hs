{-# LANGUAGE TypeFamilies #-}
-- |
-- maintainer: Junji Hashimoto <junji.hashimoto@gree.net>
module Test.Hspec.Contrib.Retry (retryWith) where

import           Test.Hspec.Core.Spec

data Retry a = Retry Int a

instance Example a => Example (Retry a) where
  type Arg (Retry a) = Arg a
  evaluateExample (Retry n example) a b c
    | n > 1 = do
        result <- safeEvaluateExample example a b c
        case result of
          Result _ Success{} -> return result
          Result _ Pending{} -> return result
          Result _ Failure{} -> retry
    | otherwise = evaluateExample example a b c
    where
      retry = evaluateExample (Retry (pred n) example) a b c

-- | Retry evaluating example that may be failed until success.
retryWith :: Int
          -- ^ number of retries, when this number is 1, just evaluate example and finish.
          -> a
          -- ^ retried example
          -> Retry a
          -- ^ Retry is instance of Example.
retryWith = Retry
