#!/usr/bin/env bash

# Run testsuite on all Phrasebook languages individually and show report

PREFIX="$(dirname $0)/phrasebook/Phrasebook"
LANGS="Bul Cat Chi Dan Dut Eng Est Fin Fre Ger Hin Ita Jpn Lav Nor Pol Ron Snd Spa Swe Tha Urd"

stack build --test --bench --no-run-tests --no-run-benchmarks

PASS=0
FAIL=0
TOTAL=0
for LANG in $LANGS; do
  printf "$LANG "
  stack test gf:test:lpgf --test-arguments="${PREFIX} ${LANG}" > /dev/null 2>&1
  if [ $? -eq 0 ]; then
    printf "\033[0;32m✓\033[0m\n"
    ((PASS=PASS+1))
  else
    printf "\033[0;31m✗\033[0m\n"
    ((FAIL=FAIL+1))
  fi
  ((TOTAL=TOTAL+1))
done

echo
echo "Passed ${PASS} | Failed ${FAIL} | Total ${TOTAL}"
