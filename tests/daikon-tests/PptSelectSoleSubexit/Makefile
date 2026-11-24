###########################################################################

# Read Makefile.common for what all of this means

TRACE_SOURCE:=stored
OUTPUT_TYPE = daikon
NO_MERGE=1
NO_CSHARP=1
NO_JML=1

# Select the entry, numbered exit, and OBJECT program points but not the
# combined exit points.
PRINT_INVARIANTS_OPTIONS = --ppt-select-pattern='ENTER|EXIT[0-9]|OBJECT'

include ../../Makefile.common
