###########################################################################

# Read Makefile.common for what all of this means

TRACE_SOURCE:=stored
OUTPUT_TYPE = daikon
NO_MERGE=1
NO_CSHARP=1
NO_JML=1

include ../../Makefile.common

# daikon.diff.Diff must also classify the program point by its declared type,
# as Daikon does.
txt-diff: txt-invdiff-diff

# By default, daikon.diff.Diff compares only entries and combined exits, and
# this program point is neither, despite its name.  With -p, it compares every
# program point.
txt-invdiff-diff: ${PROJECT_FILE}.txt-invdiff.diff;
${PROJECT_FILE}.txt-invdiff: ${INV_FILE}
	-rm -f $@-failed
	(echo "=== daikon.diff.Diff" > $@ \
	  && $(RUN_JAVA) $(USER_JAVA_FLAGS) daikon.diff.Diff $(INV_FILE) >> $@ \
	  && echo "=== daikon.diff.Diff -p" >> $@ \
	  && $(RUN_JAVA) $(USER_JAVA_FLAGS) daikon.diff.Diff -p $(INV_FILE) >> $@) \
	  || (mv $@ $@-failed && false)
