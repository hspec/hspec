echo -e ':set prog spec\n:main --help' | cabal exec -- ghci -v0 example/Spec.hs  > help.txt
