#!/bin/bash

# Get some system info for debugging.
for file in /etc/*release; do
  echo "$file"
  cat "$file"
  echo
done
gcc --version
make --version
ldd --version
find /lib/ | grep -s "libc-" || true
find /lib64/ | grep -s "libc-" || true
echo "end of system info"
echo ""

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars compile daikon.jar

echo "test-kvasir.sh is running kvasir and DynComp tests"

# Running Kvasir tests here may seem redundant with the fjalar project's Travis
# build; however, it means that they are run on each branch and pull request.

# Get correct version of Kvasir/fjalar
if [ ! -d ../fjalar ]; then
  if [ -d "/tmp/$USER/git-scripts" ]; then
    (cd "/tmp/$USER/git-scripts" && git pull -q) > /dev/null 2>&1
  else
    mkdir -p "/tmp/$USER"
    (cd "/tmp/$USER" && (git clone --depth 1 -q https://github.com/plume-lib/git-scripts.git || (sleep 1m && git clone --depth 1 -q https://github.com/plume-lib/git-scripts.git)))
  fi
  "/tmp/$USER/git-scripts/git-clone-related" codespecs fjalar
fi

# The Valgrind configure script fails if SHELLOPTS is defined.
export -n SHELLOPTS

make -f tests/kvasir-tests/Makefile.common show-os

make kvasir

make -C tests/dyncomp-tests regression-tests
#make -C tests/kvasir-tests regression-tests
make MPARG=-j1 -C tests/kvasir-tests regression-tests
