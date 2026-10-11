# DAIKONDIR is the clone of the 'daikon' project.
ifeq ($(origin DAIKONDIR), undefined)
  DAIKONDIR := $(realpath $(dir $(lastword ${MAKEFILE_LIST}))../..)
endif
# Make splits pathnames at spaces, so DAIKONDIR must not contain any.
ifeq ($(strip ${DAIKONDIR}),)
  $(error DAIKONDIR is empty; set it to the top-level directory of your daikon clone)
else ifneq ($(words ${DAIKONDIR}),1)
  $(error DAIKONDIR must be a directory whose pathname contains no spaces; DAIKONDIR="${DAIKONDIR}")
endif

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
	-java -ea -cp .:${DAIKON_CLASSPATH} jtb.JavaParserTest $< > $<.log 2>&1
	-diff -u $< $@ > $@.diff 2>&1
	@cat $@.diff
