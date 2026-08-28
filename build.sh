#!/usr/bin/env bash
set -ux
trap 'pkill --parent $$' EXIT
while sleep 1
do
  inotifywait --quiet -e modify -e delete elm.json src test &
  elm make --output=elm.js src/Main.elm
  elm make --output=test/check.js test/Check.elm && node test/run.js
  wait %inotifywait
done
