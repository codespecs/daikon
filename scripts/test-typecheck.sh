#!/bin/bash

# This is the "typecheck" job of the pull request.

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars
make compile daikon.jar

utils/plume-scripts/git-clone-related typetools checker-framework
(cd ../checker-framework && source checker/bin-devel/build.sh)
CHECKERFRAMEWORK=$(realpath ../checker-framework)
export CHECKERFRAMEWORK

make -C java typecheck
