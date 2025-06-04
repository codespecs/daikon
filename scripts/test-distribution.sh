#!/bin/sh

# Download the distribution and run "make distribution-check".

SCRIPTDIR="$(cd "$(dirname "$0")" > /dev/null 2>&1 && pwd -P)"

# Fail the whole script if any command fails
set -e
# Fail if any command in a pipeline fails
#set -o pipefail
# Echo commands before executing them
set -x

# Use default URL if environment variable is not set by caller.
# Caller might do: DAIKONBASEURL=http://plse.cs.washington.edu/staging-daikon
DAIKONBASEURL=${DAIKONBASEURL:-http://plse.cs.washington.edu/daikon}

# Convert OSTYPE from a shell variable to a system environment variable.
export OSTYPE

mkdir -p ~/tmp
cd ~/tmp

# If this fails because the URL doesn't exist and it's a staging URL, then
# maybe you are not in the middle of creating a release.
# DAIKONVERSION=$(wget -q "$DAIKONBASEURL/download/doc/VERSION" -O - | xargs echo -n)
DAIKONVERSION=$(curl --fail -s "$DAIKONBASEURL/download/doc/VERSION" | xargs echo -n)

rm -rf "daikon-$DAIKONVERSION.tar.gz" "daikon-$DAIKONVERSION"
# wget "$DAIKONBASEURL/download/daikon-$DAIKONVERSION.tar.gz"
curl --fail -O "$DAIKONBASEURL/download/daikon-$DAIKONVERSION.tar.gz"

tar xzf "daikon-$DAIKONVERSION.tar.gz"
cd "daikon-$DAIKONVERSION"
DAIKONDIR=$(pwd)
export DAIKONDIR
if [ ! -f "${SCRIPTDIR}"/daikon.bashrc ]; then
  echo "\$0=$0"
  echo "SCRIPTDIR=${SCRIPTDIR}"
  echo "DAIKONDIR=${DAIKONDIR}"
fi
. "${SCRIPTDIR}"/daikon.bashrc
make distribution-check
