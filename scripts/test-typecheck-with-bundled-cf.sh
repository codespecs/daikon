#!/bin/bash

# This is the "typecheck" job of the pull request.
# It uses the bundled version of the Checker Framework: the one commited to the Daikon repostiory.

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars
make compile daikon.jar

unset CHECKERFRAMEWORK

make -C java --jobs="$(getconf _NPROCESSORS_ONLN)" typecheck
