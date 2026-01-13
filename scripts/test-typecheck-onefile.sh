#!/bin/bash
# Use bash, not sh, because of `set -o pipefail`.

# This is the "typecheck-onefile" job of the pull request.
# It uses the HEAD version of the Checker Framework: the latest commit in the GitHub repository.

set -e
set -o pipefail
export SHELLOPTS

## Useful for debugging and sometimes for interpreting the script.
# # Output lines of this script as they are read.
# set -o verbose
# # Output expanded lines of this script as they are executed.
# set -o xtrace

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
cd "${SCRIPT_DIR}"/..

make showvars
make compile daikon.jar

make update-git-scripts

# Use a version of the Checker Framework cloned from a GitHub
# repository, NOT the version checked in at java/lib/checker-framework/.
.utils/git-scripts/git-clone-related typetools checker-framework

(cd ../checker-framework && ./gradlew assembleForJavac --console=plain -Dorg.gradle.internal.http.socketTimeout=60000 -Dorg.gradle.internal.http.connectionTimeout=60000)
CHECKERFRAMEWORK=$(realpath ../checker-framework)
export CHECKERFRAMEWORK

make -C java typecheck-nullness-onefile
