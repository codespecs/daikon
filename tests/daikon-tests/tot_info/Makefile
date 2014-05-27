###########################################################################

# Read Makefile.common for what all of this means

TRACE_SOURCE:=stored
OUTPUT_TYPE = daikon
NO_MERGE=1
NO_CSHARP=1

include ../../Makefile.common

# To turn off Simplify, put this in the project's own Makefile, after
# the "include ../../Makefile.common" line.
DAIKON_OPTIONS:=$(subst --suppress_redundant,,${DAIKON_OPTIONS}) --config config.txt
# FIXME: config.txt only used because V3 has a memory use problem
