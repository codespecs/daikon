#!/bin/bash

# This is the "misc" job of the pull request.

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars compile daikon.jar

# Code style & quality
make -C java error-prone

# Code formatting
make -C java check-format

# Documentation
if java -version 2>&1 | grep -q '"1.8'; then
  # Java version 8
  if ! grep -q Ubuntu /etc/os-release; then
    # Not Ubuntu
    SKIP_JAVADOC=1
  fi
fi
if [ ! -z ${SKIP_JAVADOC+x} ]; then
  echo Skipping javadoc because of https://bugs.openjdk.java.net/browse/JDK-8215542
  exit
else
  make javadoc doc-all

  git -C /tmp/plume-scripts pull > /dev/null 2>&1 \
    || git -C /tmp clone --depth 1 -q https://github.com/plume-lib/plume-scripts.git
  (make -C java requireJavadocPrivate > /tmp/warnings.txt 2>&1) || true
  /tmp/plume-scripts/ci-lint-diff /tmp/warnings.txt
fi
