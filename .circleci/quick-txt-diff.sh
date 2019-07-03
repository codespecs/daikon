#!/bin/bash

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars compile daikon.jar

make dyncomp-jdk
make -C tests MPARG=-Otarget quick-txt-diff results
