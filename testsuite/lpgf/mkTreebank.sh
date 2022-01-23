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

echo "Compiling PGF"
gf --make --output-dir="$(DIRNAME $ABSNAME)" $ABSNAME*.gf

echo "Writing $SCRIPT"
: > $SCRIPT
while read tree; do
  echo "linearize -treebank $tree | write_file -file=$TREEBANK -append"  >> "$SCRIPT"
  echo "put_string \"\" | write_file -file=$TREEBANK -append"  >> "$SCRIPT"
done < $TREES

echo "Writing $TREEBANK"
: > $TREEBANK
gf --crun "$ABSNAME.pgf" < "$SCRIPT" > /dev/null

echo "Removing $SCRIPT"
rm "$SCRIPT"
