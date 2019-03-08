#!/bin/bash

# Fail the whole script if any command fails
set -e

# get linux version details
lsb_release -a

export SHELLOPTS

make showvars compile daikon.jar
