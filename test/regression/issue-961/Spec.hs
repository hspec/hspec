-- Regression test for hspec#961:
--   JobQueue.cancelQueue retained one Async (and its underlying TSO) per spec
--   item for the lifetime of the run, growing O(n) in the number of test
--   cases. After the fix, completed workers self-deregister, so the queue
--   only holds in-flight jobs.
--
-- Reproducer: a synthetic suite with nested describes (similar shape to a
-- real test tree) where each leaf does a small amount of work. Run with
-- `+RTS -hT -RTS` to produce a heap profile by closure type; the
-- accompanying run.sh inspects Spec.hp to validate that retention of
-- worker-owned closure types stays roughly constant in the suite size.

module Main (main) where

import           Control.Monad (forM_)
import           Test.Hspec

outer, middle, leaf :: Int
outer = 50
middle = 20
leaf = 50

main :: IO ()
main = hspec $ describe "issue-961" $
  forM_ [1 .. outer] $ \ i ->
    describe ("group-" ++ show i) $
      forM_ [1 .. middle] $ \ j ->
        describe ("subgroup-" ++ show j) $
          forM_ [1 .. leaf] $ \ k ->
            it ("test-" ++ show i ++ "-" ++ show j ++ "-" ++ show k) $ do
              let xs = map (\ x -> x * x + i + j + k) [1 .. 200 :: Int]
              sum xs `shouldBe` sum xs
