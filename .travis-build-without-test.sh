#!/bin/bash

# Fail the whole script if any command fails
set -e

export SHELLOPTS

make showvars compile daikon.jar
