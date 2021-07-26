#!/usr/bin/env sh

# For a given grammar, compile into canonical format,
# then ensure that the canonical format itself is compilable.

if [ $# -lt 1 ]; then
  echo "Please specify concrete modules to test with, e.g.:"
  echo "./run-on-grammar.sh ../../../gf-contrib/foods/FoodsEng.gf  ../../../gf-contrib/foods/FoodsFin.gf"
  exit 2
fi

FAILURES=0

for CNC_PATH in "$@"; do
  CNC_FILE=$(basename "$CNC_PATH")
  stack run -- --batch --output-format=canonical_gf "$CNC_PATH"
  if [ $? -ne 0 ]; then
    echo "Failed to compile into canonical"
    FAILURES=$((FAILURES+1))
    continue
  fi

  stack run -- --batch "canonical/$CNC_FILE"
  if [ $? -ne 0 ]; then
    echo "Failed to compile canonical"
    FAILURES=$((FAILURES+1))
  fi
done

# Summary
if [ $FAILURES -ne 0 ]; then
  echo "Failures: $FAILURES"
  exit 1
else
  echo "All tests passed"
fi
