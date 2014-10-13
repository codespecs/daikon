###########################################################################

# Read Makefile.common for what all of this means

# Generating the .dtrace takes 7 minutes on my (slow!) laptop as of 7/12/2005

MAIN_CLASS:=DataStructures.BinaryHeap

#ESC_ON:=1

## INSTRUMENT:=infer
## #INSTRUMENT:=explicit
## #INSTR_FILES:=DataStructures/StackAr.java

# INSTRUMENT_OBSERVERS:=1

include ../../Makefile.common

# USER_DAIKON_OPTIONS:=--ppt-select-pattern="^DataStructures.Binary[:\.]"

#USER_CLUSTER_OPTIONS:=-a km -k 5

instrument-files-revise:
	echo "DataStructures/BinaryHeap.java" >| ${INST_LIST_FILE}
