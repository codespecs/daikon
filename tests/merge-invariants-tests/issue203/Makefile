#
# Tests that MergeInvariants produces the same invariants as Daikon run on all
# the trace files at once, when variables that are incomparable at the OBJECT
# ppt are equal in only one of the input files.  In Run1, this.x == this.y; in
# Run2, they differ.  The OBJECT ppt's comparability depends only on its
# children's comparability (see PptTopLevel.coarsenComparability), not on the
# data, so the merged invariants should be the same as the invariants produced
# from both trace files directly.
#
# The *-spinfo targets test the same thing, with splitters.  MergeInvariants
# is given Issue203Incomparable.decls, in which this.x and this.y are
# incomparable at the method ppts, but they are comparable in the runs.

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

# A failing diff is left in a *-failed.diff file for inspection.
diffs: daikon.txt.diff daikon-spinfo.txt.diff
daikon.txt.diff: combined.inv.gz merge.inv.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants combined.inv.gz > combined.txt
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants merge.inv.gz > merge.txt
	diff -u combined.txt merge.txt > daikon.txt-failed.diff || (cat daikon.txt-failed.diff && false)
	mv daikon.txt-failed.diff daikon.txt.diff

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

daikon-spinfo.txt.diff: combined-spinfo.inv.gz merge-spinfo.inv.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants combined-spinfo.inv.gz > combined-spinfo.txt
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants merge-spinfo.inv.gz > merge-spinfo.txt
	diff -u combined-spinfo.txt merge-spinfo.txt > daikon-spinfo.txt-failed.diff || (cat daikon-spinfo.txt-failed.diff && false)
	mv daikon-spinfo.txt-failed.diff daikon-spinfo.txt.diff

merge-spinfo.inv.gz: ${DAIKON} Issue203Incomparable.decls Issue203.spinfo Run1-spinfo.inv.gz Run2-spinfo.inv.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.MergeInvariants -o merge-spinfo.inv.gz \
	  Issue203Incomparable.decls Issue203.spinfo Run1-spinfo.inv.gz Run2-spinfo.inv.gz

combined-spinfo.inv.gz: ${DAIKON} Issue203.decls Issue203.spinfo Run1.dtrace.gz Run2.dtrace.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon -o combined-spinfo.inv.gz ${DAIKON_ARGS} \
	  Issue203.decls Issue203.spinfo Run1.dtrace.gz Run2.dtrace.gz

Run%-spinfo.inv.gz: ${DAIKON} Issue203.decls Issue203.spinfo Run%.dtrace.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon -o $@ ${DAIKON_ARGS} \
	  Issue203.decls Issue203.spinfo Run$*.dtrace.gz

update-goals:

.PHONY: clean
clean:
	/bin/rm -rf *.inv.gz *.diff *.txt
