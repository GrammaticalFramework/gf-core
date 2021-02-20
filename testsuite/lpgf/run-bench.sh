#!/usr/bin/env bash

if [ $# -lt 1 ] ; then
  echo "Specify language"
  exit 1
fi

PREFIX="$(dirname $0)/phrasebook/Phrasebook"
TREES="${PREFIX}-10000.trees"
FLAGS="+RTS -T -RTS"

stack build --test --bench --no-run-tests --no-run-benchmarks

printf "\n-- COMPILE --\n\n"
DEBUG=1 stack bench --benchmark-arguments "compile pgf ${PREFIX}${1}.gf ${FLAGS}"
printf "\n"
DEBUG=1 stack bench --benchmark-arguments "compile lpgf ${PREFIX}${1}.gf ${FLAGS}"

printf "\n-- RUN -- \n\n"
stack bench --benchmark-arguments "run pgf Phrasebook.pgf ${TREES} ${FLAGS}"
printf "\n"
stack bench --benchmark-arguments "run pgf2 Phrasebook.pgf ${TREES} ${FLAGS}"
printf "\n"
stack bench --benchmark-arguments "run lpgf Phrasebook.lpgf ${TREES} ${FLAGS}"
