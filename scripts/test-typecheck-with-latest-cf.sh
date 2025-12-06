#!/bin/bash
# Use bash, not sh, because of `set -o pipefail`.

# This is the "typecheck" job of the pull request.
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

if test -d utils/git-scripts/.git; then
  (cd utils/git-scripts && (git pull -q || (sleep 1m && (git pull || true))))
elif ! test -d utils/git-scripts; then
  (mkdir -p utils && cd utils && (git clone -q --depth 1 https://github.com/plume-lib/git-scripts.git || (sleep 1m && git clone -q --depth 1 https://github.com/plume-lib/git-scripts.git)))
fi

# Use a version of the Checker Framework cloned from a GitHub
# repository, NOT the version checked in at java/lib/checker-framework/.
utils/git-scripts/git-clone-related typetools checker-framework

(cd ../checker-framework && ./gradlew assembleForJavac --console=plain -Dorg.gradle.internal.http.socketTimeout=60000 -Dorg.gradle.internal.http.connectionTimeout=60000)
CHECKERFRAMEWORK=$(realpath ../checker-framework)
export CHECKERFRAMEWORK

# Under CI, there are two CPUs, but limit to 1 to avoid out-of-memory error.
if [ -n "$("${SCRIPT_DIR}"/is-ci.sh)" ]; then
  make -C java typecheck
else
  num_jobs="$(nproc || sysctl -n hw.ncpu || getconf _NPROCESSORS_ONLN || echo 1)"
  make -C java --jobs="$num_jobs" typecheck
fi
