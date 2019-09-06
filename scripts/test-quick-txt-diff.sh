#!/bin/bash

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars compile daikon.jar

make dyncomp-jdk
MAKE_VERSION=$(make --version 2>&1 | head -1)
if [[ $MAKE_VERSION =~ "GNU Make 4" ]]; then
  MPARG_ARG="MPARG=-Otarget"
fi
make -C tests $MPARG_ARG quick-txt-diff results
