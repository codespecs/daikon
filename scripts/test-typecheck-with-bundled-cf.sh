#!/bin/bash
# Bash, not sh, because of `set -o pipefail`.

# This is the "typecheck" job of the pull request.
# It uses the bundled version of the Checker Framework: the one commited to the Daikon repostiory.

set -e
set -o pipefail
export SHELLOPTS

## Useful for debugging and sometimes for interpreting the script.
# # Output lines of this script as they are read.
# set -o verbose
# # Output expanded lines of this script as they are executed.
# set -o xtrace

make showvars
make compile daikon.jar

unset CHECKERFRAMEWORK

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
cd "${SCRIPT_DIR}"/..

# Under CI, there are two CPUs, but limit to 1 to avoid out-of-memory error.
if [ -n "$("${SCRIPT_DIR}"/is-ci.sh)" ]; then
  make -C java typecheck
else
  num_jobs="$(nproc || sysctl -n hw.ncpu || getconf _NPROCESSORS_ONLN || echo 1)"
  make -C java --jobs="$num_jobs" typecheck
fi
