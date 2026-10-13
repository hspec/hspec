#!/bin/bash
set -o errexit
cd `dirname "$0"`

./generate.sh

ghc -O1 -no-link -itest $(find test/ -name 'C*Spec.hs')
time -p timeout 20s ghc -O1 -c -fforce-recomp -itest test/Spec.hs
