#
# Tests that MergeInvariants, given a version 2 decls file, produces the same
# invariants as Daikon run on all the trace files at once.  The decls file
# declares only numbered exits, so MergeInvariants must create the combined
# exit, and it must build the program point hierarchy from the declarations'
# parent records.
#
# Test "decls" uses no splitters.  Test "spinfo" also uses the splitters in
# DeclsFile.spinfo, so it checks the conditional program points too.

TESTS = decls spinfo
include $(dir $(lastword $(MAKEFILE_LIST)))../Makefile.common

RUNS = Run1 Run2

decls-merge.inv.gz: ${DAIKON} DeclsFile.decls ${RUNS:=.inv.gz}
	${DAIKON_JAVA} daikon.MergeInvariants -o $@ DeclsFile.decls ${RUNS:=.inv.gz}

decls-daikon.inv.gz: ${DAIKON} DeclsFile.decls ${RUNS:=.dtrace.gz}
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} DeclsFile.decls ${RUNS:=.dtrace.gz}

${RUNS:=.inv.gz}: %.inv.gz: ${DAIKON} DeclsFile.decls %.dtrace.gz
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} DeclsFile.decls $*.dtrace.gz

spinfo-merge.inv.gz: ${DAIKON} DeclsFile.decls DeclsFile.spinfo ${RUNS:=-spinfo.inv.gz}
	${DAIKON_JAVA} daikon.MergeInvariants -o $@ DeclsFile.decls DeclsFile.spinfo \
	  ${RUNS:=-spinfo.inv.gz}

spinfo-daikon.inv.gz: ${DAIKON} DeclsFile.decls DeclsFile.spinfo ${RUNS:=.dtrace.gz}
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} DeclsFile.decls DeclsFile.spinfo \
	  ${RUNS:=.dtrace.gz}

${RUNS:=-spinfo.inv.gz}: %-spinfo.inv.gz: ${DAIKON} DeclsFile.decls DeclsFile.spinfo %.dtrace.gz
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} DeclsFile.decls DeclsFile.spinfo \
	  $*.dtrace.gz
