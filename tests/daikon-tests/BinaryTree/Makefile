###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=DataStructures.BinarySearchTreeTester
#MAIN_CLASS:=DataStructures.BinarySearchTreeTester # for esc-annotate-paper
#MAIN_CLASS:=DataStructures.BinarySearchTreeTester # for esc-annotate-paper
#MAIN_CLASS:=DataStructures.BinarySearchTreeTester  #improved test suite for clustering

ESC_ON:=1

INSTRUMENT:=infer
#INSTRUMENT:=explicit
#INSTR_FILES:=DataStructures/StackAr.java

# INSTRUMENT_OBSERVERS:=1

include ../../Makefile.common

# USER_DAIKON_OPTIONS:=--ppt-select-pattern="^DataStructures.BinarySearch[:\.]"

#USER_CLUSTER_OPTIONS:=-a km -k 5

instrument-files-revise:
	echo "DataStructures/BinarySearchTree.java" >| ${INST_LIST_FILE}
