#!/bin/bash

# Fail the whole script if any command fails
set -e

echo "Don't use .travis-build-without-test.sh; instead run: make compile daikon.jar"

# Print system information, for debugging.
for file in /etc/*release; do echo "$file"; cat "$file"; echo; done

export SHELLOPTS

make showvars compile daikon.jar
