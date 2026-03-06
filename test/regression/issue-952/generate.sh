#!/bin/bash

set -o errexit

count=0

for a in {0..9}; do
  for b in {0..9}; do
    for c in {0..9}; do
      if [ "$count" -ge 1000 ]; then
        exit 0
      fi

      dir="test/A$a/B$b"
      file="$dir/C${c}Spec.hs"

      mkdir -p "$dir"

      cat > "$file" <<EOF
module A$a.B$b.C${c}Spec where

import Test.Hspec

spec :: Spec
spec = do
  it "should be True" $ do
    True \`shouldBe\` True
EOF

      count=$((count + 1))
    done
  done
done
