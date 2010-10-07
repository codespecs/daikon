###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=DataStructures.StackArTester
#MAIN_CLASS:=DataStructures.StackArTesterOrig # for esc-annotate-paper
#MAIN_CLASS:=DataStructures.StackArTesterPlus # for esc-annotate-paper
#MAIN_CLASS:=DataStructures.StackArTesterAug  #improved test suite for clustering
ESC_ON:=1
DAIKON_OPTIONS    = --config config.txt

INSTRUMENT:=infer
#INSTRUMENT:=explicit
#INSTR_FILES:=DataStructures/StackAr.java

# INSTRUMENT_OBSERVERS:=1

#CHICORY_USER_FLAGS = --purity-file=/scratch2/jhp/tmp/kuzmina/stack.pure

include ../../Makefile.common

instrument-files-revise:
	echo "DataStructures/StackAr.java" >| ${INST_LIST_FILE}
