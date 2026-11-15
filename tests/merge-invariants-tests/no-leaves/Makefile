#
# Tests that MergeInvariants reports an error, rather than writing output with
# no invariants, when its decls file contains no leaf of the dataflow
# hierarchy.

TESTS =
include $(dir $(lastword $(MAKEFILE_LIST)))../Makefile.common

RUNS = Run1 Run2

diffs: no-leaves-error.txt

${RUNS:=.inv.gz}: %.inv.gz: ${DAIKON} Point.decls %.dtrace.gz
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} Point.decls $*.dtrace.gz

# MergeInvariants must fail, and its output must say why.
no-leaves-error.txt: ${DAIKON} NoLeaves.decls ${RUNS:=.inv.gz}
	! ${DAIKON_JAVA} daikon.MergeInvariants -o no-leaves-merge.inv.gz NoLeaves.decls \
	  ${RUNS:=.inv.gz} > $@ 2>&1
	grep -q "No leaf program point" $@ || (cat $@ && false)
