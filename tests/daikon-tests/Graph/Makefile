###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=MapQuick1.PS4TestDriver

ESC_ON:=1

# For some unknown reason the JVM may load two classes in a different order
# on different runs.  Thus, we skip the DtraceDiff comparison.
SKIP_DTRACE_DIFF := 1

# This is unnecessary when we have instrument-files-revise
# USER_DAIKON_OPTIONS:=--ppt-select-pattern="^MapQuick1\.Graph[:\.]"

USER_CLUSTER_OPTIONS:=-a km -k 5
include ../../Makefile.common

instrument-files-revise:
	echo "MapQuick1/Graph.java" >| ${INST_LIST_FILE}

# must come after the include to get SOURCES
MAIN_ARGS:=${SOURCES}/MapQuick/ps4-tests
