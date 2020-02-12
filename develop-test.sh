#!/usr/bin/env bash
hpack
trap reset EXIT
ghcid \
  --test=:"main ${1}" \
  --command='ghci -Wall -ghci-script=.ghci-test -j -Wwarn +RTS -N8 -A128m -qn4 -RTS' \
  --warnings
