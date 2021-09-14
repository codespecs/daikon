###########################################################################

# Read Makefile.common for what all of this means

# DAIKONDIR is the clone of the 'daikon' project.
DAIKONDIR ?= $(realpath $(dir $(lastword $(MAKEFILE_LIST)))../../..)

MAIN_CLASS:=misc.PurityTester

CHICORY_USER_FLAGS = --purity-file=${DAIKONDIR}/tests/daikon-tests/purity/purity.pure

include ../../Makefile.common

instrument-files-revise:
	echo "misc/Purity.java" >| ${INST_LIST_FILE}
