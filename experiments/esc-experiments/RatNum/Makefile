###########################################################################

# Read ../Makefile-fora-project for what these declarations mean

PROJECT:=RatNum
TRACE_NAME=RatNum.dtrace

PACKAGE=PolyCalc

SOURCE_UNCHANGED:=${PACKAGE}/RatPolyTest.java ${PACKAGE}/RatPoly.java ${PACKAGE}/RatTermVec.java ${PACKAGE}/RatTerm.java
SOURCE_TO_DFEJ:=${PACKAGE}/RatNum.java

MAIN_RUN:=junit.textui.TestRunner ${PACKAGE}.RatNumTest

include ../Makefile-fora-project

#DFEJ_OPTIONS:=${DFEJ_OPTIONS} -daikon_depth=6
