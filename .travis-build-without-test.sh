#!/bin/bash

# Fail the whole script if any command fails
set -e

# get linux version details
lsb_release -a || true
# quick and dirty way to get info and avoid python 2/3 problems with lsb_release
cat /etc/*release || true

export SHELLOPTS

make showvars compile daikon.jar
