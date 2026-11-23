#
# Tests that MergeInvariants produces the same invariants as Daikon run on all
# the trace files at once, for a :::POINT program point (a leaf of the dataflow
# hierarchy) that SplitPoint.spinfo splits.  MergeInvariants must merge the
# invariants at the conditional program points of the leaf.
#
# Test "decls" gives MergeInvariants a decls file and the splitters.  Test
# "template" gives it no decls file, so it builds the program points from the
# .inv files.  Test "remerge" gives it no decls file and merges the output of
# test "decls" (on two of the runs) with the third run, so it checks that
# MergeInvariants's output can itself be merged.

TESTS = decls template remerge
include $(dir $(lastword $(MAKEFILE_LIST)))../Makefile.common

RUNS = Run1 Run2 Run3

# Daikon run on all the trace files at once; the expected output of every test.
all-daikon.inv.gz: ${DAIKON} SplitPoint.decls SplitPoint.spinfo ${RUNS:=.dtrace.gz}
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} SplitPoint.decls SplitPoint.spinfo \
	  ${RUNS:=.dtrace.gz}
decls-daikon.inv.gz template-daikon.inv.gz remerge-daikon.inv.gz: all-daikon.inv.gz
	cp $< $@

${RUNS:=.inv.gz}: %.inv.gz: ${DAIKON} SplitPoint.decls SplitPoint.spinfo %.dtrace.gz
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} SplitPoint.decls SplitPoint.spinfo \
	  $*.dtrace.gz

decls-merge.inv.gz: ${DAIKON} SplitPoint.decls SplitPoint.spinfo ${RUNS:=.inv.gz}
	${DAIKON_JAVA} daikon.MergeInvariants -o $@ SplitPoint.decls SplitPoint.spinfo \
	  ${RUNS:=.inv.gz}

template-merge.inv.gz: ${DAIKON} ${RUNS:=.inv.gz}
	${DAIKON_JAVA} daikon.MergeInvariants -o $@ ${RUNS:=.inv.gz}

Run12-merge.inv.gz: ${DAIKON} SplitPoint.decls SplitPoint.spinfo Run1.inv.gz Run2.inv.gz
	${DAIKON_JAVA} daikon.MergeInvariants -o $@ SplitPoint.decls SplitPoint.spinfo \
	  Run1.inv.gz Run2.inv.gz

remerge-merge.inv.gz: ${DAIKON} Run12-merge.inv.gz Run3.inv.gz
	${DAIKON_JAVA} daikon.MergeInvariants -o $@ Run12-merge.inv.gz Run3.inv.gz
