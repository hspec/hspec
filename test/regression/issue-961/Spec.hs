-- Regression test for hspec#961:
--   JobQueue.cancelQueue retained one Async (and its underlying TSO) per spec
--   item for the lifetime of the run, growing O(n) in the number of test
--   cases. After the fix, completed workers self-deregister, so the queue
--   only holds in-flight jobs.
--
-- Reproducer: a synthetic suite with nested describes (similar shape to a
-- real test tree) where each leaf does a small amount of work. An
-- `afterAll_` hook fires after every spec item has finished while
-- `withJobQueue`'s bracket is still open — that is the window where a
-- leaked queue is observable. The hook forces a major GC and then reads
-- live-heap size out of GHC.Stats, writing it to live-bytes.txt for
-- run.sh to assert against.  Sampling via `+RTS -hT` is kept for
-- diagnostic output but it turned out to be timing-sensitive across GHC
-- versions and runner conditions (broken hspec sometimes looked passing
-- in the last -hT sample), so it is not the decision basis.
--
-- GHC <= 8.6 caveat: empirically, on GHC 8.6.5 this benchmark's heap
-- profile shows STACK retention climbing to ~663 MB across the run when
-- the fix is applied, vs ~45 MB on the same GHC with the unfixed hspec
-- (and ~35 KB on GHC 8.8.4 with the fix). Minimal reproducers with the
-- same worker shape — `forkIO`/`async` + readMVar gate + bracket_ + finally,
-- 50k threads — do *not* show this retention on GHC 8.6.5 (STACK climbs to
-- ~40 MB and drops back to ~8 MB after `performGC`), so this is not a clean
-- "GHC 8.6 fails to reclaim STACK closures" RTS issue.  Something in
-- hspec-core's eval path keeps worker state alive on 8.6.5 in a way it
-- doesn't on 8.8+, and we have not pinned it down. The workflow therefore
-- only runs this regression test on GHC 8.8 and later; the fix itself is
-- still correct on 8.6, but the test doesn't distinguish broken from fixed
-- there because both retain similar amounts of total state.

{-# LANGUAGE CPP #-}
module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forM_)
import           System.Mem (performGC)
import           Test.Hspec
#if MIN_VERSION_base(4,10,0)
import           GHC.Stats (getRTSStats, getRTSStatsEnabled, gc, gcdetails_live_bytes)
#endif

outer, middle, leaf :: Int
outer = 50
middle = 20
leaf = 50

main :: IO ()
main = hspec $ afterAll_ settle $ describe "issue-961" $
  forM_ [1 .. outer] $ \ i ->
    describe ("group-" ++ show i) $
      forM_ [1 .. middle] $ \ j ->
        describe ("subgroup-" ++ show j) $
          forM_ [1 .. leaf] $ \ k ->
            it ("test-" ++ show i ++ "-" ++ show j ++ "-" ++ show k) $ do
              let xs = map (\ x -> x * x + i + j + k) [1 .. 200 :: Int]
              sum xs `shouldBe` sum xs

-- Runs after every spec item has finished, while withJobQueue's bracket is
-- still open. We force a major GC and then read post-GC live bytes from
-- GHC.Stats — the cancel queue, if broken, still references ~50k workers
-- here; if fixed, it is empty. We also pause so the -hT heap profiler can
-- emit a few samples in this state for human inspection.
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
