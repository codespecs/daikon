#!/bin/bash

# This is the "misc" job of the pull request.

set -e
set -o pipefail
set -o verbose
set -o xtrace
export SHELLOPTS

make compile daikon.jar

if [ -d "/tmp/$USER/plume-scripts" ] ; then
  (cd "/tmp/$USER/plume-scripts" && git pull -q) > /dev/null 2>&1
else
  mkdir -p "/tmp/$USER" && (cd "/tmp/$USER" && git clone --depth 1 -q https://github.com/plume-lib/plume-scripts.git)
fi

# Code formatting
ls -l java/daikon/config
echo "---------------- ParameterDoclet.java ----------------
cat java/daikon/config/ParameterDoclet.java
echo "---------------- end of ParameterDoclet.java ----------------
chmod +w java/daikon/confug/ParameterDoclet.java java/daikon/confug/InvariantDoclet.java
make -C java reformat
echo "---------------- ParameterDoclet.java ----------------
cat java/daikon/config/ParameterDoclet.java
echo "---------------- end of ParameterDoclet.java ----------------

make -C java check-format
