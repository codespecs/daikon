###########################################################################

# Read Makefile.common for what all of this means

mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))
DAIKONDIR := $(abspath $(current_dir)/../../..)

MAIN_CLASS:=misc.PurityTester

CHICORY_USER_FLAGS = --purity-file=${DAIKONDIR}/tests/daikon-tests/purity/purity.pure

include ../../Makefile.common

instrument-files-revise:
	echo "misc/Purity.java" >| ${INST_LIST_FILE}
