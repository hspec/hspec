#!/bin/bash
#
# Regression test for hspec#961.
#
# Runs the synthetic 50k-test suite and asserts that post-GC live heap
# during the afterAll_ window stays bounded. The decision is based on
# GHC.Stats.getRTSStats — read from inside the test, written to
# live-bytes.txt — because heap profile sampling via -hT turned out to be
# timing-sensitive across GHC versions / runner conditions (the last -hT
# sample could land in a moment where the broken cancel queue happened to
# not dominate the snapshot). getRTSStats after `performGC` gives a
# deterministic post-GC heap size at the exact window where the bug shows.
#
# On the broken JobQueue, ~50k worker TSOs/STACKs are kept alive through the
# afterAll_ hook; live bytes climb well past the threshold. With the fix,
# completed workers self-deregister and the queue is empty by the time the
# hook runs.
#
# -hT output is still captured and summarised for diagnostic visibility but
# is not the pass/fail signal.

set -o errexit
cd `dirname "$0"`

LIVE_THRESHOLD_BYTES=$((20 * 1024 * 1024))

ghc -threaded -rtsopts -fforce-recomp Spec.hs
./Spec -f silent +RTS -hT -T -i0.05 -RTS

# Diagnostic only: summarise the -hT profile so we can see, in CI logs,
# what TSO/STACK/Async retention looked like at peak and at exit.
awk '
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
    printf "issue-961: -hT max TSO+STACK+Async  = %d bytes at sample %s (TSO=%d STACK=%d Async=%d)\n", \
      max_total, max_time, max_tso, max_stack, max_async
    printf "issue-961: -hT last non-empty sample at %s: total %d bytes (TSO=%d STACK=%d Async=%d)\n", \
      last_time, last_tso + last_stack + last_async, last_tso, last_stack, last_async
  }
' Spec.hp

if [ ! -f live-bytes.txt ]; then
  echo "issue-961: FAIL — live-bytes.txt was not written (afterAll_ hook did not run?)"
  rm -f Spec.o Spec.hi Spec Spec.hp
  exit 1
fi

LIVE=$(cat live-bytes.txt)
echo "issue-961: GHC.Stats post-GC live bytes = $LIVE (threshold $LIVE_THRESHOLD_BYTES)"

case "$LIVE" in
  ''|*[!0-9]*)
    echo "issue-961: SKIP — GHC.Stats not available on this GHC (value '$LIVE')"
    rm -f Spec.o Spec.hi Spec Spec.hp live-bytes.txt
    exit 0
    ;;
esac

if [ "$LIVE" -ge "$LIVE_THRESHOLD_BYTES" ]; then
  echo "issue-961: FAIL — cancel queue retained completed workers"
  rm -f Spec.o Spec.hi Spec Spec.hp live-bytes.txt
  exit 1
fi

echo "issue-961: PASS"
rm -f Spec.o Spec.hi Spec Spec.hp live-bytes.txt
