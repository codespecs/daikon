#!/bin/bash

# ROOT=$TRAVIS_BUILD_DIR
# cd $ROOT

# Same as in Jenkins; should abstract out
# make -C java very-clean
make showvars compile daikon.jar javadoc
make -C doc
make -C java dcomp_rt.jar
make -C tests all
