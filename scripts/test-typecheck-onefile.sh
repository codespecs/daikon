#!/bin/bash

# This is the "typecheck-onefile" job of the pull request.

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars
make compile daikon.jar

# Use a version of the Checker Framework cloned from a GitHub
# repository, NOT the version checked in at java/lib/checker-framework/.
utils/git-scripts/git-clone-related typetools checker-framework
# shellcheck disable=SC1091 # file does not exist relative to the script
(cd ../checker-framework && source checker/bin-devel/build.sh)
CHECKERFRAMEWORK=$(realpath ../checker-framework)
export CHECKERFRAMEWORK

make -C java typecheck-nullness-onefile
