#!/usr/bin/env bash
set -e
if [ $# -lt 1 ]; then
  echo "Must specify trees file"
  exit 1
fi
TREES=$1
ABSNAME="${1%.*}"
TREEBANK="$ABSNAME.treebank"

# echo "read_file -file=$TREES -lines -tree | linearize -treebank | write_file -file=$TREEBANK" | gf --run $ABSNAME*.gf

: > $TREEBANK
while read tree; do
  echo "linearize -treebank $tree | write_file -file=$TREEBANK -append" | gf --run $ABSNAME*.gf > /dev/null
  echo "" >> $TREEBANK
done < $TREES

echo "Wrote $TREEBANK"
