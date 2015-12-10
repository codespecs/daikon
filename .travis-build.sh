#!/bin/bash -v

# TODO: The tests ought to work even if $DAIKONDIR is not set.
export DAIKONDIR=`pwd`

make nightly-test
