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
DAIKONDIR ?= $(realpath $(dir $(lastword $(MAKEFILE_LIST)))../../..)

DAIKON_ARGS = '--ppt-omit-pattern=^org\.' --no_text_output --no_show_progress --config_option daikon.Debug.show_stack_trace=true
DAIKON = ${DAIKONDIR}/java/daikon/Daikon.class

DAIKON_CLASSPATH := ${DAIKONDIR}/java:${DAIKONDIR}/java/lib/*:${DAIKONDIR}/daikon.jar

diffs: daikon.txt.diff
daikon.txt.diff: drawlet.inv.gz merge.inv.gz
	java -cp $(DAIKON_CLASSPATH) -ea -Xmx3600m daikon.PrintInvariants drawlet.inv.gz > drawlet.txt
	java -cp $(DAIKON_CLASSPATH) -ea -Xmx3600m daikon.PrintInvariants merge.inv.gz > merge.txt
	diff -u drawlet.txt merge.txt > daikon.txt.diff
	@cat daikon.txt.diff

# Build the merged version of the invariant file from the individual inv files
merge: merge.inv.gz
merge.inv.gz: $(DAIKON) TS_AllDrawletUnitTests.inv.gz \
	          TS_DrawletFunctionalTest.inv.gz TS_InputEventHandlerTests.inv.gz
	java -cp $(DAIKON_CLASSPATH) -ea -Xmx3600m daikon.MergeInvariants -o merge.inv.gz \
	  TS_AllDrawletUnitTests.inv.gz TS_DrawletFunctionalTest.inv.gz \
	  TS_InputEventHandlerTests.inv.gz \


# Build the combined inv file directly from the trace files
daikon : drawlet.inv.gz
drawlet.inv.gz : $(DAIKON) TS_AllDrawletUnitTests.dtrace.gz \
	TS_DrawletFunctionalTest.dtrace.gz TS_InputEventHandlerTests.dtrace.gz
	java -cp $(DAIKON_CLASSPATH) -ea -Xmx3600m daikon.Daikon -o drawlet.inv.gz $(DAIKON_ARGS) \
	  TS_AllDrawletUnitTests.dtrace.gz \
	  TS_DrawletFunctionalTest.dtrace.gz \
	  TS_InputEventHandlerTests.dtrace.gz

TS_AllDrawletUnitTests.inv.gz: $(DAIKON) TS_AllDrawletUnitTests.dtrace.gz
	java -cp $(DAIKON_CLASSPATH) -ea -Xmx3600m daikon.Daikon $(DAIKON_ARGS) \
	  TS_AllDrawletUnitTests.dtrace.gz

TS_DrawletFunctionalTest.inv.gz: $(DAIKON) TS_DrawletFunctionalTest.dtrace.gz
	java -cp $(DAIKON_CLASSPATH) -ea -Xmx3600m daikon.Daikon $(DAIKON_ARGS) \
	  TS_DrawletFunctionalTest.dtrace.gz

TS_InputEventHandlerTests.inv.gz: $(DAIKON) TS_InputEventHandlerTests.dtrace.gz
	java -cp $(DAIKON_CLASSPATH) -ea -Xmx3600m daikon.Daikon $(DAIKON_ARGS) \
	  TS_InputEventHandlerTests.dtrace.gz

#
# Test targets
#

# Options for testing
#
#PPT_OPTION	= '--ppt-select-pattern=defaultHandleSize|CanvasHandle:::CLASS'
#DEBUG_OPTION := --dbg daikon.print
#DEBUG_OPTION := $(DEBUG_OPTION) --dbg daikon.Progress
#TRACK_OPTION = --track 'LowerBound<com.rolemodelsoft.drawlet.basics.CanvasHandle.HANDLE_SIZE>@defaultHandleSize'

# Run Daikon on all three input trace files
# Note that this puts the output invariant file in test.inv.gz so it doesn't
# overwrite the correct invariant file
test_d:
	java -cp $(DAIKON_CLASSPATH) -ea -Xmx3600m daikon.Daikon \
	  $(PPT_OPTION) \
	  $(DEBUG_OPTION) \
	  $(TRACK_OPTION) \
	  TS_AllDrawletUnitTests.dtrace.gz \
	  TS_DrawletFunctionalTest.dtrace.gz \
	  TS_InputEventHandlerTests.dtrace.gz \
	  -o test.inv.gz

# run PrintInvariants on the merge invariant file
test_pm:
	java -cp $(DAIKON_CLASSPATH) -ea -Xmx3600m daikon.PrintInvariants \
	  $(PPT_OPTION) \
	  $(DEBUG_OPTION) \
	  $(TRACK_OPTION) \
	  merge.inv.gz

# run MergeInvariants on the three inv files
test_m:
	java -cp $(DAIKON_CLASSPATH) -ea -Xmx3600m daikon.MergeInvariants \
	  $(DEBUG_OPTION) \
	  TS_AllDrawletUnitTests.inv.gz TS_DrawletFunctionalTest.inv.gz \
	  TS_InputEventHandlerTests.inv.gz \

# run Daikon on the first dtrace file
test_d1:
	java -cp $(DAIKON_CLASSPATH) -ea -Xmx3600m daikon.PrintInvariants \
	  $(PPT_OPTION) \
	  $(DEBUG_OPTION) \
	  $(TRACK_OPTION) \
	  TS_AllDrawletUnitTests.inv.gz

# run Daikon on the second dtrace file
test_d2:
	java -cp $(DAIKON_CLASSPATH) -ea -Xmx3600m daikon.Daikon
	  $(PPT_OPTION) \
	  $(DEBUG_OPTION) \
	  $(TRACK_OPTION) \
	  TS_DrawletFunctionalTest.dtrace.gz

update-goals:

clean:
	/bin/rm -rf *.inv.gz *.diff *.txt

show-vars:
	@echo PPT_OPTION = $(PPT_OPTION)
	@echo TRACK_OPTION = $(TRACK_OPTION)
	@echo DEBUG_OPTION = $(DEBUG_OPTION)
	@echo CLASSPATH    = $(CLASSPATH)
	@echo DAIKON_CLASSPATH    = $(DAIKON_CLASSPATH)
