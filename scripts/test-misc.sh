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
if [ -n "${SKIP_JAVADOC+x}" ]; then
  echo Skipping javadoc because of https://bugs.openjdk.java.net/browse/JDK-8215542
  exit
else
  make javadoc doc-all

  if [ -d "/tmp/$USER/plume-scripts" ] ; then
    (cd "/tmp/$USER/plume-scripts" && git pull -q) > /dev/null 2>&1
  else
    mkdir -p "/tmp/$USER" && git -C "/tmp/$USER" clone --depth 1 -q https://github.com/plume-lib/plume-scripts.git
  fi
  (make -C java requireJavadocPrivate > /tmp/warnings.txt 2>&1) || true
  "/tmp/$USER/plume-scripts/ci-lint-diff" /tmp/warnings.txt
fi
