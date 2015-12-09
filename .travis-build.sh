#!/bin/bash -v

# TODO: this should work even if $DAIKONDIR is not set.
export DAIKONDIR=`pwd`

make nightly-test
