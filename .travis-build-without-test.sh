#!/bin/bash

# Fail the whole script if any command fails
set -e

echo "Don't use .travis-build-without-test.sh; instead run: make compile daikon.jar"

# Print OS version info.
cat /etc/*release || true

export SHELLOPTS

make showvars compile daikon.jar
