# The directory containing this file.  Makefile.jdk is found relative to it,
# not via DAIKONDIR, which may name a different Daikon tree.
THIS_MAKEFILE_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
# DAIKONDIR is the clone of the 'daikon' project.
ifeq ($(origin DAIKONDIR), undefined)
  DAIKONDIR := $(realpath ${THIS_MAKEFILE_DIR}../..)
endif
include ${THIS_MAKEFILE_DIR}../Makefile.jdk

DAIKON_CLASSPATH := ${DAIKONDIR}/java:${DAIKONDIR}/java/lib/*:${DAIKONDIR}/daikon.jar

tests :=  AbstractList17.java AnAbstractClass.java ConstructorTest.java Failure3.java ForStmt.java HelloWorld.java InnerStatic.java PowerOfTwo.java QueueAr.java RightShiftExample.java TimeZone.java

results := $(tests:.java=.java-parsed)

.PHONY: all test clean

all:
	 ${MAKE} clean
	 ${MAKE} run-tests

run-tests: ${results}
test: run-tests

clean:
	rm -f *.java-parsed *.diff *.log

%.java-parsed : %.java
	-${JAVA} -ea -cp .:${DAIKON_CLASSPATH} jtb.JavaParserTest $< > $<.log 2>&1
	-diff -u $< $@ > $@.diff 2>&1
	@cat $@.diff
