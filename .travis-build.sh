#!/bin/bash -v

# Fail the whole script if any command fails
set -e

export SHELLOPTS

# TODO: The tests ought to work even if $DAIKONDIR is not set.
export DAIKONDIR=`pwd`

make nightly-test

