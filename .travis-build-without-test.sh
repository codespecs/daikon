#!/bin/bash

# Fail the whole script if any command fails
set -e

echo "Don't use .travis-build-without-test.sh; instead run: make compile daikon.jar"

# quick and dirty way to get version info and avoid python 2/3 problems with lsb_release
cat /etc/*release || true

export SHELLOPTS

make showvars compile daikon.jar
