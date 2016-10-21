#!/bin/bash
set -o errexit
cd `dirname "$0"`
ghc -fforce-recomp Spec.hs

./Spec -f silent > result & pid=$!
sleep 1
kill -SIGINT $pid
wait $pid || output=$(cat result)

rm Spec.o Spec.hi Spec result

if [ "$output" == "ran finalizer" ]; then
  echo SUCCESS
  exit 0
else
  echo FAILURE
  exit 1
fi
