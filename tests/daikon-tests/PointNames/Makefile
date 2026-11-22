###########################################################################

# Read Makefile.common for what all of this means

TRACE_SOURCE:=stored
OUTPUT_TYPE = daikon
NO_MERGE=1
NO_CSHARP=1
NO_JML=1

include ../../Makefile.common

# InvariantChecker, DaikonSimple, and daikon.diff.Diff must also classify the
# program points by their declared types, as Daikon does.
txt-diff: inv-checker-diff txt-simple-diff txt-invdiff-diff

# The common %.diff rule compares ${PROJECT_FILE}.txt-simple to its goal file every time.
txt-simple-diff: ${PROJECT_FILE}.txt-simple.diff;
${PROJECT_FILE}.txt-simple: ${DECLS_FILES} ${DTRACE_FILES}
	-rm -f $@-failed
	($(RUN_JAVA) $(USER_JAVA_FLAGS) daikon.DaikonSimple \
	  --config_option daikon.Daikon.quiet=true \
	  $(DECLS_FILES) $(DTRACE_FILES) > $@) || (mv $@ $@-failed && false)

# By default, daikon.diff.Diff compares only entries and combined exits, and
# none of these program points is either.  With -p, it compares every program
# point, and --ignore_exitNN ignores only numbered exits, which these program
# points are not.
txt-invdiff-diff: ${PROJECT_FILE}.txt-invdiff.diff;
${PROJECT_FILE}.txt-invdiff: ${INV_FILE}
	-rm -f $@-failed
	(echo "=== daikon.diff.Diff" > $@ \
	  && $(RUN_JAVA) $(USER_JAVA_FLAGS) daikon.diff.Diff $(INV_FILE) >> $@ \
	  && echo "=== daikon.diff.Diff -p --ignore_exitNN" >> $@ \
	  && $(RUN_JAVA) $(USER_JAVA_FLAGS) daikon.diff.Diff -p --ignore_exitNN $(INV_FILE) >> $@) \
	  || (mv $@ $@-failed && false)
