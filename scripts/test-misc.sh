#!/bin/bash

# This is the "misc" job of the pull request.

# Halt on error
set -e

set -o pipefail
export SHELLOPTS

## Useful for debugging and sometimes for interpreting the script.
# # Output lines of this script as they are read.
# set -o verbose
# # Output expanded lines of this script as they are executed.
# set -o xtrace

echo "HEAD=$(git rev-parse HEAD)"

make compile daikon.jar

# Code style & quality
make -C java error-prone
make -C java check-format || (make -C java reformat && git --no-pager diff && /bin/false)

make plume-scripts-update || true
# Under CI, there are two CPUs, but limit to 1 to avoid out-of-memory error.
if [ -n "$(.plume-scripts/is-ci.sh)" ]; then
  num_jobs=1
else
  num_jobs="$(nproc || sysctl -n hw.ncpu || getconf _NPROCESSORS_ONLN || echo 1)"
fi
make -k style-check --jobs="${num_jobs}"

# Documentation
if java -version 2>&1 | grep -q '"1.8'; then
  # Java version 8
  if ! grep -q Ubuntu /etc/os-release; then
    # Not Ubuntu
    SKIP_JAVADOC=1
  fi
fi
if [ -n "${SKIP_JAVADOC+x}" ]; then
  echo Skipping javadoc because of https://bugs.openjdk.java.net/browse/JDK-8215542
  exit
else

  make javadoc doc-all

  # For refactorings that touch a lot of code that you don't understand, create
  # top-level file SKIP-REQUIRE-JAVADOC.  Delete it after the pull request is merged.
  if [ -f SKIP-REQUIRE-JAVADOC ]; then
    echo "Skipping requireJavadoc because file SKIP-REQUIRE-JAVADOC exists."
  else
    # The `api-private` and `requireJavadoc` commands are separate to avoid
    # assuming that they both produce absolute filenames or both produce filenames
    # relative to the same directory.  Always run both to avoid one masking
    # failures in the other.
    status=0
    reason=""
    # The `grep -v` prevents the make target failure from throwing off prefix guessing.
    (make -C java api-private 2>&1 | grep -v "^Makefile:[0-9]*: recipe for target 'api-private' failed" > "/tmp/$USER/ap-warnings.txt") || true
    if ! ".plume-scripts/ci-lint-diff" "/tmp/$USER/ap-warnings.txt"; then
      status=1
      reason="$reason
target 'api-private' failed"
    fi
    (make -C java requireJavadoc 2>&1 | grep -v "^Makefile:[0-9]*: recipe for target 'requireJavadoc' failed" > "/tmp/$USER/rj-warnings.txt") || true
    if ! ".plume-scripts/ci-lint-diff" "/tmp/$USER/rj-warnings.txt"; then
      status=1
      reason="$reason
target 'requireJavadoc' failed"
    fi
    if [ $status -ne 0 ]; then
      echo "$reason"
      echo "See output above"
      exit 1
    fi
  fi
fi
