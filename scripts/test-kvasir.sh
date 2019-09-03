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

# The Valgrind configure script fails if SHELLOPTS is defined.
export -n SHELLOPTS
make -f tests/kvasir-tests/Makefile.common show-os

make kvasir

make -C tests/dyncomp-tests regression-tests
make -C tests/kvasir-tests regression-tests
