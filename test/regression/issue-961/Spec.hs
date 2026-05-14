-- Regression test for hspec#961:
--   JobQueue.cancelQueue retained one Async (and its underlying TSO) per spec
--   item for the lifetime of the run, growing O(n) in the number of test
--   cases. After the fix, completed workers self-deregister, so the queue
--   only holds in-flight jobs.
--
-- Reproducer: a synthetic suite with nested describes (similar shape to a
-- real test tree) where each leaf does a small amount of work. An
-- `afterAll_` hook performs GC and sleeps for a second, which keeps the
-- cancel queue alive (it lives until `withJobQueue`'s bracket fires) while
-- all worker threads have already completed — that is exactly the window
-- where a leaked queue is observable. Run with `+RTS -hT -RTS` to produce a
-- heap profile by closure type; the accompanying run.sh inspects Spec.hp to
-- validate that worker-state retention stays bounded.
--
-- GHC <= 8.6 caveat: the fix adds an MVar-based gate at worker startup so
-- the parent can register the worker's Async before the worker proceeds.
-- On GHC <= 8.6 the RTS retains the STACK closures saved for these blocked
-- threads well past the point where they resume and finish; the heap profile
-- shows STACK growing into the hundreds of MB across 50k workers regardless
-- of cancel-queue state. From GHC 8.8 onward STACK closures of resumed
-- threads are reclaimed promptly and the regression signal is clean. The
-- workflow therefore only runs this test on GHC 8.8 and later.

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forM_)
import           System.Mem (performGC)
import           Test.Hspec

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
-- still open. We force a GC and pause so the heap profiler captures several
-- samples in this state; that's the window where a broken cancel queue is
-- still holding worker references and a fixed one is not.
settle :: IO ()
settle = do
  performGC
  threadDelay 1000000
