#!/bin/bash

# This is the "typecheck" job of the pull request.

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

export JAVA_HOME=${JAVA_HOME:-`which javac|xargs readlink -f|xargs dirname|xargs dirname`}

make showvars compile daikon.jar

(cd .. && git clone https://github.com/typetools/checker-framework.git)
(cd ../checker-framework && source checker/bin-devel/build.sh)
export CHECKERFRAMEWORK=`realpath ../checker-framework`

make -C java typecheck
