#!/usr/bin/env bash
set -e
if [ $# -lt 1 ]; then
  echo "Must specify trees file"
  exit 1
fi
ABSNAME="${1%.*}"
echo "read_file -file=$1 -lines -tree | linearize -treebank | write_file -file=$ABSNAME.treebank" | gf --run $ABSNAME*.gf
echo "Wrote $ABSNAME.treebank"
echo "(you will have to add newlines separating the trees manually)"
