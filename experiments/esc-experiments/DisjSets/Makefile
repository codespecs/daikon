###########################################################################

# Read ../Makefile-fora-project for what these declarations mean

PROJECT:=DisjSets
TRACE_NAME=DisjSets.dtrace

PACKAGE=DataStructures

SOURCE_UNCHANGED:=${PACKAGE}/DisjSetsTest.java
SOURCE_TO_DFEJ:=${PACKAGE}/DisjSets.java

MAIN_RUN:=${PACKAGE}.DisjSetsTest

EXTRA_MERGE_SUFFIX:=.fixed

merge: pre-merge-fixer

pre-merge-fixer:
	@echo
	@echo \#\#\# pre-merge-fixer
	@echo
	rm -f ${PROJECT}.txt-esc${EXTRA_MERGE_SUFFIX}
	cat ${PROJECT}.txt-esc | perl pre-merge-fixer.pl > ${PROJECT}.txt-esc${EXTRA_MERGE_SUFFIX}

include ../Makefile-fora-project
