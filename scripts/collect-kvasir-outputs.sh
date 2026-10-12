#!/bin/bash

# Copies the outputs of failing Kvasir and DynComp tests into a directory, for
# uploading as a CI artifact.  Also copies the Valgrind configure log, which
# helps to diagnose a failure to build Kvasir.
# Usage: collect-kvasir-outputs.sh DESTDIR

set -e
set -o pipefail

dest="$1"
mkdir -p "$dest"
shopt -s nullglob

TEST_ROOTS=(tests/kvasir-tests tests/dyncomp-tests)

# Prints the directories of tests that failed (they have a non-empty .diff
# file) or that did not finish (they have a .out file but no .diff file).
failed_test_dirs() {
  find "${TEST_ROOTS[@]}" -name '*.diff' -size +0 -printf '%h\n'
  find "${TEST_ROOTS[@]}" -name '*.out' -printf '%h\n' | while read -r dir; do
    diffs=("$dir"/*.diff)
    if [ ${#diffs[@]} -eq 0 ]; then
      echo "$dir"
    fi
  done
}

failed_test_dirs | sort -u | while read -r dir; do
  echo "Collecting outputs of $dir"
  cp --parents "$dir"/*.out "$dir"/*.diff "$dir"/*.counts "$dir"/*.comp \
    "$dir"/daikon-output/*.decls "$dir"/daikon-output/*.dtrace \
    "$dir"/daikon-output/*.inv.txt "$dir"/daikon-output/*.invs.txt \
    "$dest"
done

if [ -f fjalar/valgrind/config.log ]; then
  cp fjalar/valgrind/config.log "$dest"/valgrind-config.log
fi
