###########################################################################

# Read Makefile.common for what all of this means

# Tests configuration option daikon.Daikon.ppt_perc, which processes only the first 50% of the
# program points (in sorted order), plus the OBJECT program point, which is their parent.

TRACE_SOURCE:=stored
OUTPUT_TYPE=daikon
NO_MERGE=1
NO_CSHARP=1
NO_JML=1

include ../../Makefile.common

DAIKON_OPTIONS += --config_option daikon.Daikon.ppt_perc=50
