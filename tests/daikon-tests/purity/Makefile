###########################################################################

# Read Makefile.common for what all of this means

# DAIKONDIR is the clone of the 'daikon' project.
ifeq ($(origin DAIKONDIR), undefined)
  DAIKONDIR := $(realpath $(dir $(lastword ${MAKEFILE_LIST}))../../..)
endif
# Make splits pathnames at spaces, so DAIKONDIR must not contain any.
ifneq ($(words ${DAIKONDIR}),1)
  $(error DAIKONDIR must be a directory whose pathname contains no spaces; DAIKONDIR="${DAIKONDIR}")
endif


MAIN_CLASS:=misc.PurityTester

CHICORY_USER_FLAGS = --purity-file=${DAIKONDIR}/tests/daikon-tests/purity/purity.pure

include ../../Makefile.common

instrument-files-revise:
	echo "misc/Purity.java" >| ${INST_LIST_FILE}
