#
# Tests that MergeInvariants produces the same invariants as Daikon run on all
# the trace files at once, when variables that are incomparable at the OBJECT
# ppt are equal in only one of the input files.  In Run1, this.x == this.y; in
# Run2, they differ.  The OBJECT ppt's comparability depends only on its
# children's comparability (see PptTopLevel.coarsenComparability), not on the
# data, so the merged invariants should be the same as the invariants produced
# from both trace files directly.
#
# Test "spinfo" tests the same thing, with splitters.  MergeInvariants is given
# Issue203Incomparable.decls, in which this.x and this.y are incomparable at
# the method ppts, but they are comparable in the runs.

TESTS = issue203 spinfo
include $(dir $(lastword $(MAKEFILE_LIST)))../Makefile.common

RUNS = Run1 Run2

issue203-merge.inv.gz: ${DAIKON} ${RUNS:=.inv.gz}
	${DAIKON_JAVA} daikon.MergeInvariants -o $@ ${RUNS:=.inv.gz}

issue203-daikon.inv.gz: ${DAIKON} Issue203.decls ${RUNS:=.dtrace.gz}
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} Issue203.decls ${RUNS:=.dtrace.gz}

${RUNS:=.inv.gz}: %.inv.gz: ${DAIKON} Issue203.decls %.dtrace.gz
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} Issue203.decls $*.dtrace.gz

spinfo-merge.inv.gz: ${DAIKON} Issue203Incomparable.decls Issue203.spinfo ${RUNS:=-spinfo.inv.gz}
	${DAIKON_JAVA} daikon.MergeInvariants -o $@ Issue203Incomparable.decls Issue203.spinfo \
	  ${RUNS:=-spinfo.inv.gz}

spinfo-daikon.inv.gz: ${DAIKON} Issue203.decls Issue203.spinfo ${RUNS:=.dtrace.gz}
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} Issue203.decls Issue203.spinfo \
	  ${RUNS:=.dtrace.gz}

${RUNS:=-spinfo.inv.gz}: %-spinfo.inv.gz: ${DAIKON} Issue203.decls Issue203.spinfo %.dtrace.gz
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} Issue203.decls Issue203.spinfo \
	  $*.dtrace.gz
