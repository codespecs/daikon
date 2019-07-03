#!/bin/bash

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars compile daikon.jar

make -C tests non-txt-diff results
