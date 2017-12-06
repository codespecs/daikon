mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))
DAIKONDIR := $(abspath $(current_dir)/../..)

tests :=  AbstractList17.java AnAbstractClass.java ConstructorTest.java Failure3.java ForStmt.java HelloWorld.java InnerStatic.java PowerOfTwo.java QueueAr.java RightShiftExample.java TimeZone.java

results := $(tests:.java=.java-parsed)

all:
	 $(MAKE) clean
	 $(MAKE) run-tests

run-tests: $(results)

clean:
	rm -f *.java-parsed *.diff *.log

%.java-parsed : %.java
	-java -ea -cp .:$(DAIKONDIR)/java:$(CLASSPATH) jtb.JavaParserTest $< > $<.log 2>&1
	-diff -u $< $@ > $@.diff 2>&1
	@cat $@.diff
