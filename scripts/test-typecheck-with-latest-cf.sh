#!/bin/bash

# This is the "typecheck" job of the pull request.
# It uses the HEAD version of the Checker Framework: the latest commit in the GitHub repository.

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars
make compile daikon.jar

# Use a version of the Checker Framework cloned from a GitHub
# repository, NOT the version checked in at java/lib/checker-framework/.
utils/plume-scripts/git-clone-related typetools checker-framework
(cd ../checker-framework && ./gradlew assembleForJavac --console=plain -Dorg.gradle.internal.http.socketTimeout=60000 -Dorg.gradl    e.internal.http.connectionTimeout=60000)
CHECKERFRAMEWORK=$(realpath ../checker-framework)
export CHECKERFRAMEWORK

make -C java typecheck
