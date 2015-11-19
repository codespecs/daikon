#!/bin/bash

# ROOT=$TRAVIS_BUILD_DIR
# cd $ROOT

# Same as in Jenkins; should abstract out
# make -C java very-clean

# temp changes while debugging

# make showvars compile daikon.jar javadoc
make showvars compile daikon.jar
# make -C doc
make -C java dcomp_rt.jar
# make -C tests all
cd tests/daikon-tests/suppress02
make diffs

/usr/lib/jvm/java-8-oracle/bin/java -ea -Xmx1500m -cp ".:/usr/lib/jvm/java-8-oracle/jre/lib/rt.jar:/usr/lib/jvm/java-8-oracle/lib/tools.jar:/tmp/travis/tests/daikon-tests/suppress02:/home/travis/build/codespecs/daikon/java:/home/travis/build/codespecs/daikon/java/lib/java-getopt.jar:/home/travis/build/codespecs/daikon/java/lib/plume.jar"    daikon.Daikon -o /home/travis/build/codespecs/daikon/tests/daikon-tests/suppress02/Suppress02.spinfo.inv.gz --config config.txt --config_option daikon.split.PptSplitter.suppressSplitterErrors=false --no_text_output /tmp/travis/tests/daikon-tests/suppress02/Suppress02-dyncomp.dtrace.gz Suppress02.spinfo-static
