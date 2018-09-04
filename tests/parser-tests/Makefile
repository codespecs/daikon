DAIKONDIR := $(realpath $(dir $(lastword $(MAKEFILE_LIST)))../..)

DAIKON_CLASSPATH := ${DAIKONDIR}/java:${DAIKONDIR}/java/lib/*:${DAIKONDIR}/daikon.jar

tests :=  AbstractList17.java AnAbstractClass.java ConstructorTest.java Failure3.java ForStmt.java HelloWorld.java InnerStatic.java PowerOfTwo.java QueueAr.java RightShiftExample.java TimeZone.java

results := $(tests:.java=.java-parsed)

all:
	 $(MAKE) clean
	 $(MAKE) run-tests

run-tests: $(results)

clean:
	rm -f *.java-parsed *.diff *.log

%.java-parsed : %.java
	-java -ea -cp .:$(DAIKON_CLASSPATH) jtb.JavaParserTest $< > $<.log 2>&1
	-diff -u $< $@ > $@.diff 2>&1
	@cat $@.diff
