###########################################################################

# Read Makefile.common for what all of this means

TRACE_SOURCE:=stored
OUTPUT_TYPE = daikon
NO_MERGE=1
NO_CSHARP=1
NO_JML=1

include ../../Makefile.common

# InvariantChecker, DaikonSimple, and daikon.diff.Diff must also classify the
# program points by their declared types, as Daikon does.  None of these
# program points is an entry or a combined exit, so only the output of
# "daikon.diff.Diff -p" compares their invariants.
txt-diff: inv-checker-diff txt-simple-diff txt-invdiff-diff
