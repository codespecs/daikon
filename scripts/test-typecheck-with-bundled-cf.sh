#!/bin/bash
# Bash, not sh, because of `set -o pipefail`.

# This is the "typecheck" job of the pull request.
# It uses the bundled version of the Checker Framework: the one commited to the Daikon repostiory.

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars
make compile daikon.jar

unset CHECKERFRAMEWORK

# Under CI, there are two CPUs, but limit to 1 to avoid out-of-memory error.
if [ -n "$CIRCLECI" ] || [ -n "$AZURE_HTTP_USER_AGENT" ]; then
  make -C java typecheck
else
  num_jobs="$(nproc || sysctl -n hw.ncpu || getconf _NPROCESSORS_ONLN || echo 1)"
  make -C java --jobs="$num_jobs" typecheck
fi
