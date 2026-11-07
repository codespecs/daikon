#
# Makefile that tests MergeInvariants on tracefiles created for the
# drawlet program (from Laurentiu Radu Vanciu <radu@wayne.edu>).
#
# Note that we are just using the trace files and not rerunning chicory
# to recreate them.  If we should need to recreate them, the source
# files are in drawlet_src.zip in this directory

DAIKON_ARGS = '--ppt-omit-pattern=^org\.'
TESTS = drawlet
include $(dir $(lastword $(MAKEFILE_LIST)))../Makefile.common

TRACE_FILES = TS_AllDrawletUnitTests.dtrace.gz TS_DrawletFunctionalTest.dtrace.gz \
	TS_InputEventHandlerTests.dtrace.gz

# Build the merged version of the invariant file from the individual inv files
merge: drawlet-merge.inv.gz
drawlet-merge.inv.gz: ${DAIKON} ${TRACE_FILES:.dtrace.gz=.inv.gz}
	${DAIKON_JAVA} daikon.MergeInvariants -o $@ ${TRACE_FILES:.dtrace.gz=.inv.gz}

# Build the combined inv file directly from the trace files
daikon: drawlet-daikon.inv.gz
drawlet-daikon.inv.gz: ${DAIKON} ${TRACE_FILES}
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} ${TRACE_FILES}

TS_%.inv.gz: TS_%.dtrace.gz ${DAIKON}
	${DAIKON_JAVA} daikon.Daikon -o $@ ${DAIKON_ARGS} $<

# For debugging:
#   make TS_AllDrawletUnitTests.txt TS_AllDrawletUnitTests.txt-all \
#     TS_DrawletFunctionalTest.txt TS_DrawletFunctionalTest.txt-all \
#     TS_InputEventHandlerTests.txt TS_InputEventHandlerTests.txt-all

.PHONY: merge daikon
