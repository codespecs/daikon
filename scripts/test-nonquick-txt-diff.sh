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
make -C tests MPARG=-j1 nonquick-txt-diff results
