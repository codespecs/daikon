###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=alloy.testera.Daikon
MAIN_ARGS:=-t alloy.testera.TreeMapTester -i $(shell pwd)/TreeMap.tst
EXTRA_CLASSPATH_END:=:$(shell pwd)/test.jar

include ../../Makefile.common

#DAIKON_OPTIONS:=--noternary

instrument-files-revise:
	echo "alloy/testera/TreeMap.java" >| ${INST_LIST_FILE}

# Override default rule; unfortunately, this produces a Makefile warning.
# This overriding is necessary because the main class, testera.Daikon, is
# available only in bytecode, not .java, form.
${SCRATCH_FOR_DAIKON}/.all-files:
	echo "alloy/testera/TreeMap.java" >| $@
