###########################################################################

# Read ../Makefile-fora-project for what these declarations mean

PROJECT:=RatPoly
TRACE_NAME=RatPoly.dtrace

# eventually derive this from dfej list?
PACKAGE=PolyCalc

SOURCE_UNCHANGED:=${PACKAGE}/RatPolyTest.java ${PACKAGE}/RatTerm.java ${PACKAGE}/RatNum.java
SOURCE_TO_DFEJ:=${PACKAGE}/RatPoly.java ${PACKAGE}/RatTermVec.java ${PACKAGE}/RatTermVec2.java 

MAIN_RUN:=junit.textui.TestRunner ${PACKAGE}.RatPolyTest

include ../Makefile-fora-project

DFEJ_OPTIONS:=${DFEJ_OPTIONS} -daikon_depth=3
