###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=MapQuick1.PS3TestDriver

ESC_ON:=1

include ../../Makefile.common

instrument-files-revise:
	echo "MapQuick/WeightedNodePath.java" >| ${INST_LIST_FILE}

# must come after the include to get SOURCES
MAIN_ARGS:=${SOURCES}/MapQuick/ps3-tests
