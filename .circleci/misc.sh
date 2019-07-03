#!/bin/bash

# This is the "misc" job of the pull request.

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

export JAVA_HOME=${JAVA_HOME:-`which javac|xargs readlink -f|xargs dirname|xargs dirname`}

make showvars compile daikon.jar

# Code style & quality
make -C java error-prone

# Code formatting
make -C java check-format

# Documentation
make javadoc doc-all

# $TRAVIS_COMMIT_RANGE is empty for builds triggered by the initial commit of a new branch.
if [ -n "$TRAVIS_COMMIT_RANGE" ] ; then
  # Until https://github.com/travis-ci/travis-ci/issues/4596 is fixed, $TRAVIS_COMMIT_RANGE is a
  # good argument to `git diff` but a bad argument to `git log` (they interpret "..." differently!).
  (git diff $TRAVIS_COMMIT_RANGE > /tmp/diff.txt 2>&1) || true
  (make -C java requireJavadocPrivate > /tmp/warnings.txt 2>&1) || true
  [ -s /tmp/diff.txt ] || ([[ "${TRAVIS_BRANCH}" != "master" && "${TRAVIS_EVENT_TYPE}" == "push" ]] || (echo "/tmp/diff.txt is empty; try pulling base branch (often master) into compare branch (often feature branch)" && false))
  wget https://raw.githubusercontent.com/plume-lib/plume-scripts/master/lint-diff.py
  python lint-diff.py --strip-diff=2 --strip-lint=1 /tmp/diff.txt /tmp/warnings.txt
fi
