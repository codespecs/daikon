#!/bin/sh

# Download the distribution and run "make distribution-check".


# Fail the whole script if any command fails
set -e
# Echo commands before executing them
set -x


# Convert OSTYPE from a shell variable to a system environment variable.
export OSTYPE

mkdir -p ~/tmp
cd ~/tmp

# DAIKONVERSION=`wget -q http://plse.cs.washington.edu/staging-daikon/download/doc/VERSION -O - | xargs echo -n`
DAIKONVERSION=`curl --fail -s http://plse.cs.washington.edu/staging-daikon/download/doc/VERSION | xargs echo -n`

rm -rf daikon-$DAIKONVERSION.tar.gz daikon-$DAIKONVERSION
# wget http://plse.cs.washington.edu/staging-daikon/download/daikon-$DAIKONVERSION.tar.gz
curl --fail -O http://plse.cs.washington.edu/staging-daikon/download/daikon-$DAIKONVERSION.tar.gz

tar xzf daikon-$DAIKONVERSION.tar.gz
cd daikon-$DAIKONVERSION
export DAIKONDIR=`pwd`
source scripts/daikon.bashrc
make distribution-check
