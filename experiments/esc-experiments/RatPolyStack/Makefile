###########################################################################

# Read ../Makefile-fora-project for what these declarations mean

PROJECT:=RatPolyStack
TRACE_NAME=RatPolyStack.dtrace

# eventually derive this from dfej list?
PACKAGE=PolyCalc

SOURCE_UNCHANGED:=${PACKAGE}/RatPolyStackTest.java ${PACKAGE}/RatPoly.java ${PACKAGE}/RatTermVec.java ${PACKAGE}/RatTerm.java ${PACKAGE}/RatNum.java
SOURCE_TO_DFEJ:=${PACKAGE}/RatPolyStack.java

MAIN_RUN:=junit.textui.TestRunner ${PACKAGE}.RatPolyStackTest

include ../Makefile-fora-project
