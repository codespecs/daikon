#
# Tests that MergeInvariants produces the same invariants as Daikon run on all
# the trace files at once, when variables that are incomparable at the OBJECT
# ppt are equal in only one of the input files.  In Run1, this.x == this.y; in
# Run2, they differ.  The OBJECT ppt's comparability depends only on its
# children's comparability (see PptTopLevel.coarsenComparability), not on the
# data, so the merged invariants should be the same as the invariants produced
# from both trace files directly.

# DAIKONDIR is the clone of the 'daikon' project.
ifeq ($(origin DAIKONDIR), undefined)
  DAIKONDIR := $(realpath $(dir $(lastword ${MAKEFILE_LIST}))../../..)
endif
include ${DAIKONDIR}/tests/Makefile.jdk

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

diffs: daikon.txt.diff
daikon.txt.diff: combined.inv.gz merge.inv.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants combined.inv.gz > combined.txt
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants merge.inv.gz > merge.txt
	diff -u combined.txt merge.txt > daikon.txt.diff || (cat daikon.txt.diff && false)

# Build the merged version of the invariant file from the individual inv files
merge.inv.gz: ${DAIKON} Run1.inv.gz Run2.inv.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.MergeInvariants -o merge.inv.gz \
	  Run1.inv.gz Run2.inv.gz

# Build the combined inv file directly from the trace files
combined.inv.gz: ${DAIKON} Issue203.decls Run1.dtrace.gz Run2.dtrace.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon -o combined.inv.gz ${DAIKON_ARGS} \
	  Issue203.decls Run1.dtrace.gz Run2.dtrace.gz

Run%.inv.gz: ${DAIKON} Issue203.decls Run%.dtrace.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon -o $@ ${DAIKON_ARGS} \
	  Issue203.decls Run$*.dtrace.gz

update-goals:

.PHONY: clean
clean:
	/bin/rm -rf *.inv.gz *.diff *.txt
