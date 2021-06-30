#!/usr/bin/env sh

# https://github.com/GrammaticalFramework/gf-core/issues/100
stack run -- --batch --output-format=canonical_gf PhrasebookBul.gf
stack run -- --batch canonical/PhrasebookBul.gf

# https://github.com/GrammaticalFramework/gf-core/issues/101
stack run -- --batch --output-format=canonical_gf PhrasebookGer.gf
for s in c2 objCtrl; do
  grep VRead --after-context=216 canonical/PhrasebookGer.gf | grep "$s" > /dev/null
  if [ $? -ne 1 ]; then
    echo "$s found"
    exit 1
  fi
done

# https://github.com/GrammaticalFramework/gf-core/issues/102
stack run -- --batch --output-format=canonical_gf FoodsFin.gf
diff canonical/FoodsFin.gf ./FoodsFin.gf.gold
if [ $? -ne 0 ]; then
  echo "Compiled grammar doesn't match gold version"
  exit 1
fi
