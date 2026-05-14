#!/bin/bash
#
# Regression test for hspec#961.
#
# Runs the synthetic suite at two spec-count sizes (small ~ N_SMALL and
# large ~ N_LARGE), measures post-GC live heap in each run, and asserts
# that bytes-per-spec-item — the slope between the two — is small. A
# leaking cancel queue retains O(n) per-worker state, so the slope is
# the clean signal: broken hspec shows a multi-KB-per-test slope; fixed
# hspec shows a slope dominated by the result tree (a few hundred bytes
# per test). A flat-baseline comparison cancels out the fixed-cost
# overhead of the test harness itself, which varies by GHC version /
# runner and otherwise muddies a single-point threshold.
#
# Decision basis: GHC.Stats.getRTSStats, read inside the test's
# afterAll_ hook (where withJobQueue's bracket is still open and the
# cancel queue, if leaking, is still holding all completed workers) and
# written to live-bytes.txt. -hT heap profile output is preserved per
# run for diagnostic visibility but is not the pass/fail signal.

set -o errexit
cd `dirname "$0"`

N_SMALL=5000
N_LARGE=50000
BYTES_PER_TEST_THRESHOLD=1500

ghc -threaded -rtsopts -fforce-recomp Spec.hs

run_size () {
  local n="$1"
  local tag="$2"
  rm -f live-bytes.txt Spec.hp
  ISSUE961_N="$n" ./Spec -f silent +RTS -hT -T -i0.05 -RTS

  awk -v tag="$tag" -v n="$n" '
    BEGIN { max_total = -1 }
    /^BEGIN_SAMPLE/ { time=$2; tso=0; stack=0; async=0; have=0 }
    /^TSO[[:space:]]/                              { tso=$NF;   have=1 }
    /^STACK[[:space:]]/                            { stack=$NF; have=1 }
    /Control\.Concurrent\.Async\.Async[[:space:]]/ { async=$NF; have=1 }
    /^END_SAMPLE/ {
      sample_total = tso + stack + async
      if (sample_total > max_total) {
        max_total = sample_total
        max_time = time; max_tso = tso; max_stack = stack; max_async = async
      }
      if (have) {
        last_time = time
        last_tso = tso; last_stack = stack; last_async = async
      }
    }
    END {
      printf "issue-961: [%s N=%d] -hT max TSO+STACK+Async = %d at %s (TSO=%d STACK=%d Async=%d)\n", \
        tag, n, max_total, max_time, max_tso, max_stack, max_async
      printf "issue-961: [%s N=%d] -hT last sample at %s: total %d (TSO=%d STACK=%d Async=%d)\n", \
        tag, n, last_time, last_tso + last_stack + last_async, last_tso, last_stack, last_async
    }
  ' Spec.hp

  if [ ! -f live-bytes.txt ]; then
    echo "issue-961: FAIL — live-bytes.txt was not written for $tag run"
    exit 1
  fi
  local live
  live=$(cat live-bytes.txt)
  echo "issue-961: [$tag N=$n] GHC.Stats post-GC live bytes = $live"
  mv live-bytes.txt "live-bytes-$tag.txt"
  mv Spec.hp "Spec-$tag.hp"
}

run_size "$N_SMALL" small
run_size "$N_LARGE" large

LIVE_SMALL=$(cat live-bytes-small.txt)
LIVE_LARGE=$(cat live-bytes-large.txt)

case "$LIVE_SMALL$LIVE_LARGE" in
  *[!0-9]*)
    echo "issue-961: SKIP — GHC.Stats not available on this GHC (small='$LIVE_SMALL' large='$LIVE_LARGE')"
    rm -f Spec.o Spec.hi Spec Spec-*.hp live-bytes-*.txt
    exit 0
    ;;
esac

DIFF=$((LIVE_LARGE - LIVE_SMALL))
DN=$((N_LARGE - N_SMALL))
BYTES_PER_TEST=$((DIFF / DN))

echo "issue-961: live(N=$N_LARGE) - live(N=$N_SMALL) = $DIFF bytes over $DN tests"
echo "issue-961: slope = $BYTES_PER_TEST bytes/test (threshold $BYTES_PER_TEST_THRESHOLD bytes/test)"

rm -f Spec.o Spec.hi Spec Spec-*.hp live-bytes-*.txt

if [ "$BYTES_PER_TEST" -ge "$BYTES_PER_TEST_THRESHOLD" ]; then
  echo "issue-961: FAIL — live heap grows $BYTES_PER_TEST bytes per spec item (cancel queue leak)"
  exit 1
fi

echo "issue-961: PASS"
