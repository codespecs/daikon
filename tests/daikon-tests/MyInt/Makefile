###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=DataStructures.MyIntTester
ESC_ON:=1
DAIKON_OPTIONS    = --config config.txt

INSTRUMENT:=infer
#INSTRUMENT:=explicit
#INSTR_FILES:=DataStructures/MyInt.java

# INSTRUMENT_OBSERVERS:=1

CHICORY_USER_FLAGS = --purity-file=misc/MyInt.pure

include ../../Makefile.common

instrument-files-revise:
	echo "DataStructures/MyInt.java" >| ${INST_LIST_FILE}
