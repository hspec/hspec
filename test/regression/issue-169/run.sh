#!/bin/bash
set -o errexit
cd `dirname "$0"`
ghc -rtsopts -fforce-recomp Spec.hs
./Spec +RTS -M1.5m
rm Spec.o Spec.hi Spec
