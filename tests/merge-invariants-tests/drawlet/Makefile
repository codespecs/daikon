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
# Make splits pathnames at spaces, so DAIKONDIR must not contain any.
ifeq ($(strip ${DAIKONDIR}),)
  $(error DAIKONDIR is empty; set it to the top-level directory of your daikon clone)
else ifneq ($(words ${DAIKONDIR}),1)
  $(error DAIKONDIR must be a directory whose pathname contains no spaces; DAIKONDIR="${DAIKONDIR}")
endif

DAIKON_ARGS = '--ppt-omit-pattern=^org\.' --no_text_output --no_show_progress --config_option daikon.Debug.show_stack_trace=true
# A prerequisite that changes whenever Daikon is rebuilt: Daikon.class if the
# java/ tree is compiled, otherwise daikon.jar.  If neither exists, use
# Daikon.class, so that make reports the missing file rather than running
# java with no Daikon on the classpath.
DAIKON := $(or $(firstword $(wildcard ${DAIKONDIR}/java/daikon/Daikon.class ${DAIKONDIR}/daikon.jar)),${DAIKONDIR}/java/daikon/Daikon.class)

DAIKON_CLASSPATH := ${DAIKONDIR}/java:${DAIKONDIR}/java/lib/*:${DAIKONDIR}/daikon.jar

# If a recipe fails, delete its target, so that a failing diff does not
# leave behind a daikon.txt.diff that makes the next run appear up to date.
.DELETE_ON_ERROR:

diffs: daikon.txt.diff
daikon.txt.diff: drawlet.txt merge.txt
	diff -u drawlet.txt merge.txt > daikon.txt.diff || (cat daikon.txt.diff && false)

# Build the merged version of the invariant file from the individual inv files
merge: merge.inv.gz
merge.inv.gz: ${DAIKON} TS_AllDrawletUnitTests.inv.gz \
	          TS_DrawletFunctionalTest.inv.gz TS_InputEventHandlerTests.inv.gz
	java -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.MergeInvariants -o merge.inv.gz \
	  TS_AllDrawletUnitTests.inv.gz TS_DrawletFunctionalTest.inv.gz \
	  TS_InputEventHandlerTests.inv.gz


# Build the combined inv file directly from the trace files
daikon : drawlet.inv.gz
drawlet.inv.gz : ${DAIKON} TS_AllDrawletUnitTests.dtrace.gz \
	TS_DrawletFunctionalTest.dtrace.gz TS_InputEventHandlerTests.dtrace.gz
	java -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon -o drawlet.inv.gz ${DAIKON_ARGS} \
	  TS_AllDrawletUnitTests.dtrace.gz \
	  TS_DrawletFunctionalTest.dtrace.gz \
	  TS_InputEventHandlerTests.dtrace.gz

TS_AllDrawletUnitTests.inv.gz: ${DAIKON} TS_AllDrawletUnitTests.dtrace.gz
	java -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon ${DAIKON_ARGS} \
	  TS_AllDrawletUnitTests.dtrace.gz

TS_DrawletFunctionalTest.inv.gz: ${DAIKON} TS_DrawletFunctionalTest.dtrace.gz
	java -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon ${DAIKON_ARGS} \
	  TS_DrawletFunctionalTest.dtrace.gz

TS_InputEventHandlerTests.inv.gz: ${DAIKON} TS_InputEventHandlerTests.dtrace.gz
	java -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.Daikon ${DAIKON_ARGS} \
	  TS_InputEventHandlerTests.dtrace.gz

# For debugging:
#   make TS_AllDrawletUnitTests.txt TS_AllDrawletUnitTests.txt-all \
#     TS_DrawletFunctionalTest.txt TS_DrawletFunctionalTest.txt-all \
#     TS_InputEventHandlerTests.txt TS_InputEventHandlerTests.txt-all

%.txt: %.inv.gz ${DAIKON}
	java -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants $< > $@

%.txt-all: %.inv.gz ${DAIKON}
	java -cp ${DAIKON_CLASSPATH} -ea -Xmx7g daikon.PrintInvariants --config_option daikon.PrintInvariants.print_all=true $< > $@

update-goals:

.PHONY: diffs merge daikon update-goals clean
clean:
	/bin/rm -rf *.inv.gz *.diff *.txt *.txt-all
