###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=DataStructures.DisjSetsTest
#MAIN_CLASS:=DataStructures.DisjSetsTestOrig # for esc-specs paper
#MAIN_CLASS:=DataStructures.DisjSetsTestPlus # for esc-specs paper
#MAIN_CLASS:=DataStructures.DisjSetsTestAug  # improved test suite for clustering

DAIKON_OPTIONS:=--ppt-select-pattern="^DataStructures\.DisjSets[:\.]"

ESC_ON:=1

#USER_CLUSTER_OPTIONS:=-a km -k 5

include ../../Makefile.common

instrument-files-revise:
	echo "DataStructures/DisjSets.java" >| ${INST_LIST_FILE}
