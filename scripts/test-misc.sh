#!/bin/bash

# This is the "misc" job of the pull request.

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make compile daikon.jar

if [ -d "/tmp/$USER/plume-scripts" ]; then
  (cd "/tmp/$USER/plume-scripts" && git pull -q) > /dev/null 2>&1
else
  mkdir -p "/tmp/$USER"
  (cd "/tmp/$USER" && (git clone --depth=1 --depth 1 -q https://github.com/plume-lib/plume-scripts.git || (sleep 1m && git clone --depth=1 --depth 1 -q https://github.com/plume-lib/plume-scripts.git)))
fi

# Code style & quality
make -C java error-prone
make -C java check-format || (make -C java reformat && git diff && /bin/false)
make -k -C scripts style-check

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
    if ! "/tmp/$USER/plume-scripts/ci-lint-diff" "/tmp/$USER/ap-warnings.txt"; then
      status=1
      reason="$reason
target 'api-private' failed"
    fi
    (make -C java requireJavadoc 2>&1 | grep -v "^Makefile:[0-9]*: recipe for target 'requireJavadoc' failed" > "/tmp/$USER/rj-warnings.txt") || true
    if ! "/tmp/$USER/plume-scripts/ci-lint-diff" "/tmp/$USER/rj-warnings.txt"; then
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
