###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=Main

# Be sure to do 'make clean-output' if you switch this value; make
# doesn't know that a change here means Daikon needs to be re-run.
CALLSITE_GRAIN:=line
#CALLSITE_GRAIN:=method
#CALLSITE_GRAIN:=class

# Override sources to draw from "."
SOURCES:=$(shell pwd)

include ../../Makefile.common

DAIKON_OPTIONS:=--config config.txt
