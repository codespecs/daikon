#!/bin/bash

# ROOT=$TRAVIS_BUILD_DIR
# cd $ROOT

# Same as in Jenkins; should abstract out
make all-dist
make -C java very-clean
make showvars compile javadoc
make -C doc
make daikon.jar
make -C java dcomp_rt.jar
make -C tests clean all
