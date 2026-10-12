#
# Makefile that tests MergeInvariants on tracefiles created for the
# drawlet program (from Laurentiu Radu Vanciu <radu@wayne.edu>).  This
# should be modified into a Makefile.common if we ever create another
# program to test.
#
# Note that we are just using the trace files and not rerunning chicory
# to recreate them.  If we should need to recreate them, the source
# files are in drawlet_src.zip in this directory

# DAIKONDIR is the clone of the 'daikon' project.
ifeq ($(origin DAIKONDIR), undefined)
  DAIKONDIR := $(realpath $(dir $(lastword ${MAKEFILE_LIST}))../../..)
endif
include ${DAIKONDIR}/tests/Makefile.jdk

DAIKON_ARGS = '--ppt-omit-pattern=^org\.' --no_text_output --no_show_progress --config_option daikon.Debug.show_stack_trace=true
# A prerequisite that changes whenever Daikon is rebuilt: Daikon.class if the
# java/ tree is compiled, otherwise daikon.jar.
DAIKON := $(firstword $(wildcard ${DAIKONDIR}/java/daikon/Daikon.class ${DAIKONDIR}/daikon.jar))
ifeq (${DAIKON},)
  ifneq ($(filter-out clean update-goals,$(or ${MAKECMDGOALS},diffs)),)
    $(error Daikon is not built.  Run "make -C ${DAIKONDIR} compile")
  endif
endif

DAIKON_CLASSPATH := ${DAIKONDIR}/java:${DAIKONDIR}/java/lib/*:${DAIKONDIR}/daikon.jar

# If a recipe fails, delete its target, so that a partially written output
# file does not make the next run appear up to date.
.DELETE_ON_ERROR:

# A failing diff is left in daikon.txt-failed.diff for inspection.
diffs: daikon.txt.diff
daikon.txt.diff: drawlet.txt merge.txt
	diff -u drawlet.txt merge.txt > daikon.txt-failed.diff || (cat daikon.txt-failed.diff && false)
	mv daikon.txt-failed.diff daikon.txt.diff

# Build the merged version of the invariant file from the individual inv files
merge: merge.inv.gz
merge.inv.gz: ${DAIKON} TS_AllDrawletUnitTests.inv.gz \
	          TS_DrawletFunctionalTest.inv.gz TS_InputEventHandlerTests.inv.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.MergeInvariants -o merge.inv.gz \
	  TS_AllDrawletUnitTests.inv.gz TS_DrawletFunctionalTest.inv.gz \
	  TS_InputEventHandlerTests.inv.gz


# Build the combined inv file directly from the trace files
daikon : drawlet.inv.gz
drawlet.inv.gz : ${DAIKON} TS_AllDrawletUnitTests.dtrace.gz \
	TS_DrawletFunctionalTest.dtrace.gz TS_InputEventHandlerTests.dtrace.gz
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon -o drawlet.inv.gz ${DAIKON_ARGS} \
	  TS_AllDrawletUnitTests.dtrace.gz \
	  TS_DrawletFunctionalTest.dtrace.gz \
	  TS_InputEventHandlerTests.dtrace.gz

TS_%.inv.gz: TS_%.dtrace.gz ${DAIKON}
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon -o $@ ${DAIKON_ARGS} $<

# For debugging:
#   make TS_AllDrawletUnitTests.txt TS_AllDrawletUnitTests.txt-all \
#     TS_DrawletFunctionalTest.txt TS_DrawletFunctionalTest.txt-all \
#     TS_InputEventHandlerTests.txt TS_InputEventHandlerTests.txt-all

%.txt: %.inv.gz ${DAIKON}
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants $< > $@

%.txt-all: %.inv.gz ${DAIKON}
	${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants --config_option daikon.PrintInvariants.print_all=true $< > $@

update-goals:

.PHONY: diffs merge daikon update-goals clean
clean:
	/bin/rm -rf *.inv.gz *.diff *.txt *.txt-all
