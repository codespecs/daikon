###########################################################################

# Read Makefile.common for what all of this means

TRACE_SOURCE:=stored
OUTPUT_TYPE = daikon
NO_MERGE=1
NO_CSHARP=1
NO_JML=1

include ../../Makefile.common

# InvariantChecker and DaikonSimple must also classify the program points by
# their declared types, as Daikon does.
txt-diff: inv-checker-diff txt-simple-diff

txt-simple-diff: ${PROJECT_FILE}.txt-simple.diff;
${PROJECT_FILE}.txt-simple.diff: ${DECLS_FILES} ${DTRACE_FILES}
	$(RUN_JAVA) $(USER_JAVA_FLAGS) daikon.DaikonSimple \
	  --config_option daikon.Daikon.quiet=true \
	  $(DECLS_FILES) $(DTRACE_FILES) > ${PROJECT_FILE}.txt-simple
	diff -u ${PROJECT_FILE}.txt-simple.goal ${PROJECT_FILE}.txt-simple > $@
