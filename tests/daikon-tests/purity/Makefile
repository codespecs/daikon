###########################################################################

# Read Makefile.common for what all of this means

# DAIKONDIR is the clone of the 'daikon' project.
# PURITY_TEST_DIR is simply expanded (":=") so that it is not affected by later
# includes, which change MAKEFILE_LIST.
PURITY_TEST_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
DAIKONDIR ?= $(realpath ${PURITY_TEST_DIR}../../..)


MAIN_CLASS:=misc.PurityTester

CHICORY_USER_FLAGS = --purity-file=${DAIKONDIR}/tests/daikon-tests/purity/purity.pure

include ../../Makefile.common

instrument-files-revise:
	echo "misc/Purity.java" >| ${INST_LIST_FILE}
