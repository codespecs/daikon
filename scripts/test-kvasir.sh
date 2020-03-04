#!/bin/bash

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars compile daikon.jar

echo ".travis-build.sh is running kvasir and DynComp tests"

# Running Kvasir tests here may seem redundant with the fjalar project's Travis
# build; however, it means that they are run on each branch and pull request.

# Get correct version of Kvasir/fjalar
if [ ! -d ../fjalar ] ; then
  if [ -d "/tmp/plume-scripts" ] ; then
    (cd /tmp/plume-scripts && git pull -q) > /dev/null 2>&1
  else
    (cd /tmp && git clone --depth 1 -q https://github.com/plume-lib/plume-scripts.git)
  fi
  /tmp/plume-scripts/git-clone-related codespecs fjalar
fi

# The Valgrind configure script fails if SHELLOPTS is defined.
export -n SHELLOPTS

make -f tests/kvasir-tests/Makefile.common show-os

make kvasir

make -C tests/dyncomp-tests regression-tests
make -C tests/kvasir-tests regression-tests
