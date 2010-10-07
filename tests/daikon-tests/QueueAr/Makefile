###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=DataStructures.QueueArTester
#MAIN_CLASS:=DataStructures.QueueArTesterOrig # for esc-annotate-paper
#MAIN_CLASS:=DataStructures.QueueArTesterPlus # for esc-annotate-paper
#MAIN_CLASS:=DataStructures.QueueArTesterAug  # improved testsuite for cluster Analysis

ESC_ON:=1

DAIKON_OPTIONS:=--ppt-select-pattern="^DataStructures\.QueueAr[:\.]"

# To create implications, do "mv cluster-spinfo cluster.spinfo" and uncomment:
# USER_DAIKON_OPTIONS := --config_option daikon.PptTopLevel.pairwise_implications=true

include ../../Makefile.common

# USER_DAIKON_OPTIONS := --ppt-select-pattern="^DataStructures\.QueueAr[:\.][:d]"

#USER_CLUSTER_OPTIONS:=-a km -k 5

instrument-files-revise:
	echo "DataStructures/QueueAr.java" >| ${INST_LIST_FILE}
