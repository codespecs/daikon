#!/bin/bash

# This is the "typecheck" job of the pull request.

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make showvars
make compile daikon.jar

(cd .. && git clone https://github.com/typetools/checker-framework.git)
(cd ../checker-framework && source checker/bin-devel/build.sh)
export CHECKERFRAMEWORK=`realpath ../checker-framework`

make -C java typecheck
