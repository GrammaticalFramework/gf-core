#!/usr/bin/env bash
set -e
if [ $# -lt 1 ]; then
  echo "Must specify trees file"
  exit 1
fi
TREES=$1
ABSNAME="${1%.*}"
TREEBANK="$ABSNAME.treebank"
SCRIPT="tmp.gfs"

# echo "read_file -file=$TREES -lines -tree | linearize -treebank | write_file -file=$TREEBANK" | gf --run $ABSNAME*.gf

echo "Writing $SCRIPT"
: > $SCRIPT
while read tree; do
  echo "linearize -treebank -bind $tree | write_file -file=$TREEBANK -append"  >> "$SCRIPT"
  echo "put_string \"\" | write_file -file=$TREEBANK -append"  >> "$SCRIPT"
done < $TREES

echo "Writing $TREEBANK"
: > $TREEBANK
gf --run $ABSNAME*.gf < "$SCRIPT" | awk NF

echo "Removing $SCRIPT"
rm "$SCRIPT"
