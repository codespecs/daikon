#!/bin/bash -v

# TODO: The tests ought to work even if $DAIKONDIR is not set.
export DAIKONDIR=`pwd`
# On Travis-CI's Ubuntu 12.04 infrastructure, makeinfo does not take the
# --split=chapter command-line argument but texi2html does.
export TEXI2HTML=texi2html

make nightly-test
