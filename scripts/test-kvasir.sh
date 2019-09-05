#!/bin/bash

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

export JAVA_HOME=${JAVA_HOME:-`which javac|xargs readlink -f|xargs dirname|xargs dirname`}

make showvars compile daikon.jar

echo ".travis-build.sh is running kvasir and DynComp tests"

# Running Kvasir tests here may seem redundant with the fjalar project's Travis
# build; however, it means that they are run on each branch and pull request.

# Get correct version of Kvasir/fjalar
git -C /tmp/plume-scripts pull > /dev/null 2>&1 \
  || git -C /tmp clone --depth 1 -q https://github.com/plume-lib/plume-scripts.git
eval `/tmp/plume-scripts/ci-info codespecs`
if [ ! -d ../fjalar ] ; then
  git clone ${GIT_OPTIONS} https://github.com/$CI_ORGANIZATION/fjalar.git -b $CI_BRANCH ../fjalar
fi

# The Valgrind configure script fails if SHELLOPTS is defined.
export -n SHELLOPTS
make kvasir

make -C tests/dyncomp-tests regression-tests
make -C tests/kvasir-tests regression-tests
