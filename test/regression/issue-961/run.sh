#!/bin/bash
#
# Regression test for hspec#961.
#
# Runs the synthetic 50k-test suite under `+RTS -hT -RTS` to produce a heap
# profile by closure type. Then inspects the last non-empty sample (which
# captures retention right before process exit) and validates that the bytes
# held in TSO + STACK + Async closures — i.e. the storage the cancel queue
# would retain per worker — stays roughly constant in the suite size.
#
# On the broken JobQueue, ~50k worker TSOs/STACKs are kept alive through the
# end of the run; STACK alone reaches ~45 MB. With the fix, completed workers
# self-deregister and the same closures stay under ~5 MB total.

set -o errexit
cd `dirname "$0"`

THRESHOLD_BYTES=$((15 * 1024 * 1024))

ghc -threaded -rtsopts -fforce-recomp Spec.hs
./Spec -f silent +RTS -hT -i0.05 -RTS

awk -v threshold="$THRESHOLD_BYTES" '
  /^BEGIN_SAMPLE/ { time=$2; tso=0; stack=0; async=0; have=0 }
  /^TSO[[:space:]]/                              { tso=$NF;   have=1 }
  /^STACK[[:space:]]/                            { stack=$NF; have=1 }
  /Control\.Concurrent\.Async\.Async[[:space:]]/ { async=$NF; have=1 }
  /^END_SAMPLE/ {
    if (have) {
      last_time = time
      last_tso = tso
      last_stack = stack
      last_async = async
    }
  }
  END {
    total = last_tso + last_stack + last_async
    printf "issue-961: last non-empty sample at %s\n", last_time
    printf "issue-961:   TSO    = %d bytes\n", last_tso
    printf "issue-961:   STACK  = %d bytes\n", last_stack
    printf "issue-961:   Async  = %d bytes\n", last_async
    printf "issue-961:   total  = %d bytes (threshold %d)\n", total, threshold
    if (total >= threshold) {
      print "issue-961: FAIL — cancel queue retained completed workers"
      exit 1
    }
    print "issue-961: PASS"
  }
' Spec.hp

rm Spec.o Spec.hi Spec Spec.hp
