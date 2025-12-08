#!/bin/bash

set -e
set -o pipefail
export SHELLOPTS

## Useful for debugging and sometimes for interpreting the script.
# # Output lines of this script as they are read.
# set -o verbose
# # Output expanded lines of this script as they are executed.
# set -o xtrace

make showvars compile daikon.jar

make dyncomp-jdk
# MAKE_VERSION=$(make --version 2>&1 | head -1)
# if [[ $MAKE_VERSION =~ "GNU Make 4" ]]; then
#   MPARG_ARG="MPARG=-Otarget"
# fi
make quick-test
make -C tests MPARG=-j1 quick-txt-diff results
