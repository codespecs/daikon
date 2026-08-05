###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=ncalc
NO_MERGE=1
NO_JML=1
EXTRA_TARGET=1

# Override sources to draw from "."
SOURCES:=$(shell pwd)

include ../../Makefile.common

.PHONY: extra-target
extra-target:
	 rm -f libcalculator.so ncalc.h
	 ${JAVAC} -h . ncalc.java
	 rm -f ncalc.class
	 gcc -fPIC -I"${JAVA_HOME}/include" -I"${JAVA_HOME}/include/linux" -shared -o libcalculator.so ncalc.c
