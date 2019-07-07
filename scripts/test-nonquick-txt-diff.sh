#!/bin/bash

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

export JAVA_HOME=${JAVA_HOME:-`which javac|xargs readlink -f|xargs dirname|xargs dirname`}

make showvars compile daikon.jar

make dyncomp-jdk
make -C tests MPARG=-j1 nonquick-txt-diff results
