#!/bin/bash
# Bash, not sh, because of `set -o pipefail`.

# This is the "typecheck" job of the pull request.
# It uses the HEAD version of the Checker Framework: the latest commit in the GitHub repository.

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars
make compile daikon.jar

if test -d utils/git-scripts/.git; then
  (cd utils/git-scripts && (git pull -q || (sleep 1m && (git pull || true))))
elif ! test -d utils/git-scripts; then
  (mkdir -p utils && (git clone -q --depth 1 https://github.com/plume-lib/git-scripts.git utils/git-scripts || (sleep 1m && git clone -q --depth 1 https://github.com/plume-lib/git-scripts.git utils/git-scripts)))
fi

# Use a version of the Checker Framework cloned from a GitHub
# repository, NOT the version checked in at java/lib/checker-framework/.
utils/git-scripts/git-clone-related typetools checker-framework
(cd ../checker-framework && ./gradlew assembleForJavac --console=plain -Dorg.gradle.internal.http.socketTimeout=60000 -Dorg.gradle.internal.http.connectionTimeout=60000)
CHECKERFRAMEWORK=$(realpath ../checker-framework)
export CHECKERFRAMEWORK

if [ -z ${CIRCLECI+x} ]; then
  # $CIRCLECI is unset
  num_jobs="$(nproc || sysctl -n hw.ncpu || getconf _NPROCESSORS_ONLN || echo 1)"
else
  # $CIRCLECI is set
  num_jobs=2
fi
make -C java --jobs="$num_jobs" typecheck
