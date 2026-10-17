#
# Tests that MergeInvariants does not retain comparability that Daikon loosened
# (see PptTopLevel.makeComparable) while processing one of its input files.  In
# Run1, this.x == this.y, so the OBJECT ppt makes them comparable.  In Run2,
# they differ, so the merged invariants should not use the loosened
# comparability.  The merged invariants should be the same as the invariants
# produced from both trace files directly.

# DAIKONDIR is the clone of the 'daikon' project.
DAIKONDIR ?= $(realpath $(dir $(lastword ${MAKEFILE_LIST}))../../..)

DAIKON_ARGS = --no_text_output --no_show_progress --config_option daikon.Debug.show_stack_trace=true
DAIKON = ${DAIKONDIR}/java/daikon/Daikon.class

DAIKON_CLASSPATH := ${DAIKONDIR}/java:${DAIKONDIR}/java/lib/*:${DAIKONDIR}/daikon.jar

diffs: daikon.txt.diff
daikon.txt.diff: combined.inv.gz merge.inv.gz
	java -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants combined.inv.gz > combined.txt
	java -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants merge.inv.gz > merge.txt
	diff -u combined.txt merge.txt > daikon.txt.diff || (cat daikon.txt.diff && false)

# Build the merged version of the invariant file from the individual inv files
merge.inv.gz: ${DAIKON} Run1.inv.gz Run2.inv.gz
	java -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.MergeInvariants -o merge.inv.gz \
	  Run1.inv.gz Run2.inv.gz

# Build the combined inv file directly from the trace files
combined.inv.gz: ${DAIKON} Issue203.decls Run1.dtrace.gz Run2.dtrace.gz
	java -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon -o combined.inv.gz ${DAIKON_ARGS} \
	  Issue203.decls Run1.dtrace.gz Run2.dtrace.gz

Run%.inv.gz: ${DAIKON} Issue203.decls Run%.dtrace.gz
	java -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon -o $@ ${DAIKON_ARGS} \
	  Issue203.decls Run$*.dtrace.gz

update-goals:

.PHONY: clean
clean:
	/bin/rm -rf *.inv.gz *.diff *.txt
