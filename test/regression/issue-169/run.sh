#!/bin/bash
set -o errexit
cd `dirname "$0"`
ghc -rtsopts -fforce-recomp -packagehspec2 Spec.hs
./Spec +RTS -M1m
rm Spec.o Spec.hi Spec
