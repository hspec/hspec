{-# LANGUAGE TypeFamilies #-}
-- |
-- maintainer: Junji Hashimoto <junji.hashimoto@gree.net>
module Test.Hspec.Contrib.Retry (
  retryWith
, rollbackWith
) where

import           Test.Hspec.Core.Spec

data Retry a = Retry Int a
data Rollback a = Rollback a a

instance Example a => Example (Retry a) where
  type Arg (Retry a) = Arg a
  evaluateExample (Retry n example) a b c = do
    v <- evaluateExample example a b c
    case v of
      Success -> return v
      _ | n > 1 -> evaluateExample (Retry (pred n) example) a b c
      _ -> return v

instance Example a => Example (Rollback a) where
  type Arg (Rollback a) = Arg a
  evaluateExample (Rollback rollback example) a b c = do
    v <- evaluateExample example a b c
    case v of
      Success -> return v
      _ -> do
        _ <- evaluateExample rollback a b c
        return v

-- | Retry evaluating example that may be failed until success.
retryWith :: Int
          -- ^ number of retries, when this number is 1, just evaluate example and finish.
          -> a
          -- ^ retried example
          -> Retry a
          -- ^ Retry is instance of Example.
retryWith = Retry

-- | Rollback evaluating first example(for rollback) and second example (for test)
--
-- When second example (for test) is failed, first example is used for rollback.
--
-- When second example (for test) is passed, first example is not used.
rollbackWith :: a
             -- ^ example for rollback
             -> a
             -- ^ example for test
             -> Rollback a
             -- ^ Rollback is instance of Example.
rollbackWith = Rollback
