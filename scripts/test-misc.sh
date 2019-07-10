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

git -C /tmp/plume-scripts pull > /dev/null 2>&1 \
  || git -C /tmp clone --depth 1 -q https://github.com/plume-lib/plume-scripts.git
eval `/tmp/plume-scripts/ci-info`
(git diff $CI_COMMIT_RANGE > /tmp/diff.txt 2>&1) || true
(make -C java requireJavadocPrivate > /tmp/warnings.txt 2>&1) || true
[ -s /tmp/diff.txt ] || (echo "/tmp/diff.txt is empty for $CI_COMMIT_RANGE; try pulling base branch (often master) into compare branch (often your feature branch)" && false)
python /tmp/plume-scripts/lint-diff.py --guess-strip /tmp/diff.txt /tmp/warnings.txt
