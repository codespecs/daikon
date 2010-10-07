###########################################################################
# Test 1 for suppression.  Tests MemberSuppressionFactory1 and
# SubSequence/SubSet suppression factories.
# If output is correct, we should see
# this.theArray2[this.b] in this.theArray[] but not
# this.theArray[this.b] in this.theArray2[] because
# this.theArray is a subset of this.theArray2.


# Read Makefile.common for what all of this means

MAIN_CLASS:=misc.Suppress02
PROJECT_FILE:=Suppress02
OUTPUT_TYPE            = daikon
NO_MERGE=1

ESC_ON:=0
DAIKON_OPTIONS    = --config config.txt
include ../../Makefile.common
