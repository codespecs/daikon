###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=MapQuick1.PS4TestDriver

# USER_DAIKON_OPTS:=--ppt-select-regep="^MapQuick1\.Geo" ${USER_DAIKON_OPTIONS}

instrument-files-revise:
	@echo "MapQuick1/GeoSegment.java MapQuick1/GeoPoint.java" >| ${INST_LIST_FILE}

ESC_ON:=1

include ../../Makefile.common

#USER_CLUSTER_OPTIONS:=-a km -k 5

# must come after the include to get SOURCES
MAIN_ARGS:=${SOURCES}/MapQuick/ps4-tests
