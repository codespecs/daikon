###########################################################################

# Read ../Makefile-fora-project for what these declarations mean

PROJECT:=RatTermVec
TRACE_NAME=RatTermVec.dtrace

PACKAGE=PolyCalc

SOURCE_UNCHANGED:=${PACKAGE}/RatPolyTest.java ${PACKAGE}/RatPoly.java ${PACKAGE}/RatTerm.java ${PACKAGE}/RatNum.java
SOURCE_TO_DFEJ:=${PACKAGE}/RatTermVec.java

MAIN_RUN:=junit.textui.TestRunner ${PACKAGE}.RatPolyTest

include ../Makefile-fora-project

#DFEJ_OPTIONS:=${DFEJ_OPTIONS} -daikon_depth=6
