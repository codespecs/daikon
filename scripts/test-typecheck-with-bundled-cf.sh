#!/bin/bash
# Use bash, not sh, because of `set -o pipefail`.

# This is the "typecheck" job of the pull request.
# It uses the bundled version of the Checker Framework: the one commited to the Daikon repostiory.

# The optional first argument is "part1", "part2", or "part3", indicating which
# part of the type-checking job to run.

set -e
set -o pipefail
export SHELLOPTS

## Useful for debugging and sometimes for interpreting the script.
# # Output lines of this script as they are read.
# set -o verbose
# # Output expanded lines of this script as they are executed.
# set -o xtrace

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
cd "${SCRIPT_DIR}"/..

make showvars
make compile daikon.jar

# Use the bundled version of the Checker Framework.
unset CHECKERFRAMEWORK

# Under CI, there are two CPUs, but limit to 1 to avoid out-of-memory error.
if [ -n "$(.plume-scripts/is-ci.sh)" ]; then
  num_jobs=1
else
  num_jobs="$(nproc || sysctl -n hw.ncpu || getconf _NPROCESSORS_ONLN || echo 1)"
fi

if [ "$#" -ge 2 ]; then
  echo "$0: expected 0 or 1 arguments."
  exit 2
fi

if [ "$#" -eq 0 ]; then
  make -C java --jobs="$num_jobs" typecheck
else
  make -C java --jobs="$num_jobs" "typecheck-$1"
fi
