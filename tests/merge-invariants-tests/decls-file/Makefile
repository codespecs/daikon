#
# Tests that MergeInvariants, given a version 2 decls file, produces the same
# invariants as Daikon run on all the trace files at once.  The decls file
# declares only numbered exits, so MergeInvariants must create the combined
# exit, and it must build the program point hierarchy from the declarations'
# parent records.

# The directory containing this file.  The test infrastructure, Makefile.jdk,
# is found relative to it.  DAIKONDIR names the Daikon that is tested, which
# may be a different Daikon tree (as in ../../Makefile.common) that lacks
# Makefile.jdk.
THIS_MAKEFILE_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
# DAIKONDIR is the clone of the 'daikon' project.
ifeq ($(origin DAIKONDIR), undefined)
  DAIKONDIR := $(realpath ${THIS_MAKEFILE_DIR}../../..)
endif
include ${THIS_MAKEFILE_DIR}../../Makefile.jdk

DAIKON_ARGS = --no_text_output --no_show_progress --config_option daikon.Debug.show_stack_trace=true
# A prerequisite that changes whenever Daikon is rebuilt: Daikon.class if the
# java/ tree is compiled, otherwise daikon.jar.
DAIKON := $(firstword $(wildcard ${DAIKONDIR}/java/daikon/Daikon.class ${DAIKONDIR}/daikon.jar))
ifeq (${DAIKON},)
  ifneq ($(filter-out clean update-goals,$(or ${MAKECMDGOALS},diffs)),)
    $(error Daikon is not built.  Run "make -C ${DAIKONDIR} compile")
  endif
endif

DAIKON_CLASSPATH := ${DAIKONDIR}/java:${DAIKONDIR}/java/lib/*:${DAIKONDIR}/daikon.jar

.DELETE_ON_ERROR:

# A failing diff is left in daikon.txt-failed.diff for inspection.
diffs: daikon.txt.diff
daikon.txt.diff: combined.inv.gz merge.inv.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants combined.inv.gz > combined.txt
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants merge.inv.gz > merge.txt
	diff -u combined.txt merge.txt > daikon.txt-failed.diff || (cat daikon.txt-failed.diff && false)
	mv daikon.txt-failed.diff daikon.txt.diff

# Build the merged version of the invariant file from the individual inv files
merge.inv.gz: ${DAIKON} DeclsFile.decls Run1.inv.gz Run2.inv.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.MergeInvariants -o merge.inv.gz \
	  DeclsFile.decls Run1.inv.gz Run2.inv.gz

# Build the combined inv file directly from the trace files
combined.inv.gz: ${DAIKON} DeclsFile.decls Run1.dtrace.gz Run2.dtrace.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon -o combined.inv.gz ${DAIKON_ARGS} \
	  DeclsFile.decls Run1.dtrace.gz Run2.dtrace.gz

Run%.inv.gz: ${DAIKON} DeclsFile.decls Run%.dtrace.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon -o $@ ${DAIKON_ARGS} \
	  DeclsFile.decls Run$*.dtrace.gz

update-goals:

.PHONY: diffs update-goals clean
clean:
	/bin/rm -rf *.inv.gz *.diff *.txt
