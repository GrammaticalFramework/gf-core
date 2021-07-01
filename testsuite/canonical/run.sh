#!/usr/bin/env sh

FAILURES=0

# https://github.com/GrammaticalFramework/gf-core/issues/100
stack run -- --batch --output-format=canonical_gf grammars/PhrasebookBul.gf
stack run -- --batch canonical/PhrasebookBul.gf
if [ $? -ne 0 ]; then
  echo "Canonical grammar doesn't compile: FAIL"
  FAILURES=$((FAILURES+1))
else
  # echo "Canonical grammar compiles: OK"
  diff canonical/PhrasebookBul.gf gold/PhrasebookBul.gf
  if [ $? -ne 0 ]; then
    echo "Canonical grammar doesn't match gold version: FAIL"
    FAILURES=$((FAILURES+1))
  else
    echo "Canonical grammar matches gold version: OK"
  fi
fi

echo ""

# https://github.com/GrammaticalFramework/gf-core/issues/101
stack run -- --batch --output-format=canonical_gf grammars/PhrasebookGer.gf
# for s in c2 objCtrl; do
#   grep VRead --after-context=216 canonical/PhrasebookGer.gf | grep "$s" > /dev/null
#   if [ $? -ne 1 ]; then
#     echo "Canonical grammar contains \`$s\`: FAIL"
#     FAILURES=$((FAILURES+1))
#   else
#     echo "Canonical grammar does not contain \`$s\`: OK"
#   fi
# done
diff canonical/PhrasebookGer.gf gold/PhrasebookGer.gf
if [ $? -ne 0 ]; then
  echo "Canonical grammar doesn't match gold version: FAIL"
  FAILURES=$((FAILURES+1))
else
  echo "Canonical grammar matches gold version: OK"
fi

echo ""

# https://github.com/GrammaticalFramework/gf-core/issues/102
stack run -- --batch --output-format=canonical_gf grammars/FoodsFin.gf
diff canonical/FoodsFin.gf gold/FoodsFin.gf
if [ $? -ne 0 ]; then
  echo "Canonical grammar doesn't match gold version: FAIL"
  FAILURES=$((FAILURES+1))
else
  echo "Canonical grammar matches gold version: OK"
fi

echo ""

# Summary
if [ $FAILURES -ne 0 ]; then
  echo "Failures: $FAILURES"
  exit 1
else
  echo "All tests passed"
fi
