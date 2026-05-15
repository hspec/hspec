-- Regression test for hspec#961:
--   JobQueue.cancelQueue retained one Async (and its underlying TSO) per spec
--   item for the lifetime of the run, growing O(n) in the number of test
--   cases. After the fix, completed workers self-deregister, so the queue
--   only holds in-flight jobs.
--
-- Detection strategy: run the synthetic suite at two sizes (small and
-- large), measure post-GC live heap in each run, and assert that the
-- per-test slope (bytes-per-spec-item, computed across the two sizes) is
-- small. A leaking queue grows live heap linearly in the spec count, so
-- the slope is the clean O(n)-vs-O(1) signal we want. A single-point
-- measurement turns out to be unreliable: it conflates the leak with the
-- fixed-cost overhead of the test harness itself (result tree, hspec
-- internal state, base runtime), and the threshold for "broken" depends
-- on GHC version / runner. Slope cancels that base out.
--
-- The suite is built with nested describes (outer/middle/leaf) to roughly
-- mimic the shape of a real test tree. The leaf count scales with the
-- chosen size N so the spec count = `outer * middle * leaf` = N exactly.
-- N is taken from the ISSUE961_N environment variable, with a default
-- that matches the original 50k-test reproducer.
--
-- An `afterAll_` hook fires after every spec item has finished while
-- `withJobQueue`'s bracket is still open — that is the window where a
-- leaked queue is observable. The hook forces a major GC, reads
-- `gcdetails_live_bytes` from `GHC.Stats.getRTSStats`, and writes the
-- number to `live-bytes.txt` for run.sh to consume. `+RTS -hT` heap
-- profile output is preserved for diagnostic visibility but is not the
-- decision basis.

{-# LANGUAGE CPP #-}
module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forM_)
import           Data.Maybe (fromMaybe)
import           System.Environment (lookupEnv)
import           System.Mem (performGC)
import           Test.Hspec
import           Text.Read (readMaybe)
#if MIN_VERSION_base(4,10,0)
import           GHC.Stats (getRTSStats, getRTSStatsEnabled, gc, gcdetails_live_bytes)
#endif

-- Fixed describe-tree depth: 50 outer groups, 20 middle subgroups. Leaf
-- count is derived so that outer * middle * leaf = ISSUE961_N, which
-- lets run.sh vary N to measure per-spec-item growth.
outer, middle :: Int
outer  = 50
middle = 20

defaultN :: Int
defaultN = 50000

main :: IO ()
main = do
  n <- fromMaybe defaultN . (>>= readMaybe) <$> lookupEnv "ISSUE961_N"
  let leaf = max 1 (n `div` (outer * middle))
  hspec $ afterAll_ settle $ describe "issue-961" $
    forM_ [1 .. outer] $ \ i ->
      describe ("group-" ++ show i) $
        forM_ [1 .. middle] $ \ j ->
          describe ("subgroup-" ++ show j) $
            forM_ [1 .. leaf] $ \ k ->
              it ("test-" ++ show i ++ "-" ++ show j ++ "-" ++ show k) $ do
                -- Synthetic CPU work so workers spend non-trivial time in
                -- the action body (and therefore actually concurrently
                -- occupy the cancel queue). The shouldBe is tautological;
                -- the leak we measure is per-item, not per-failure.
                let xs = map (\ x -> x * x + i + j + k) [1 .. 200 :: Int]
                sum xs `shouldBe` sum xs

-- Runs after every spec item has finished, while withJobQueue's bracket is
-- still open. We force a major GC and then read post-GC live bytes from
-- GHC.Stats. The pause keeps the bracket open long enough for -hT to emit
-- a few diagnostic samples in this state.
settle :: IO ()
settle = do
  performGC
  performGC
#if MIN_VERSION_base(4,10,0)
  enabled <- getRTSStatsEnabled
  if enabled
    then do
      stats <- getRTSStats
      writeFile "live-bytes.txt" (show (gcdetails_live_bytes (gc stats)))
    else writeFile "live-bytes.txt" "DISABLED"
#else
  writeFile "live-bytes.txt" "UNAVAILABLE"
#endif
  threadDelay 1000000
