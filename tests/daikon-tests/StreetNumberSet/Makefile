###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=MapQuick.StreetNumberSetTest2
# Change flag in source/MapQuick/StreetNumberSetTest2.java to toggle
# original / augmented (esc-specs paper)
#MAIN_CLASS:=MapQuick.StreetNumberSetTest2Aug  # improved test suite for clustering


ESC_ON:=1

include ../../Makefile.common

#USER_CLUSTER_OPTIONS:=-a km -k 5

instrument-files-revise:
	echo "MapQuick1/StreetNumberSet.java" >| ${INST_LIST_FILE}
