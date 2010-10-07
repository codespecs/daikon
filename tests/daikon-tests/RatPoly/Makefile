###########################################################################

# Read Makefile.common for what all of this means

# To toggle original / edited test suite, edit the use_original flag
# in PolyCalc/PublicTest.java

MAIN_CLASS:=PolyCalc.PublicTest
#MAIN_CLASS:=PolyCalc.PublicTestAug


ESC_ON:=1

include ../../Makefile.common

# USER_DAIKON_OPTIONS:=--var_omit_pattern=".*wrapped.*"

# USER_DAIKON_OPTIONS:=--ppt-select-pattern="^Rat" --config config.txt

#USER_CLUSTER_OPTIONS:=-a km -k 5

instrument-files-revise:
	echo "PolyCalc/RatPoly.java PolyCalc/RatTermVec.java PolyCalc/RatTerm.java PolyCalc/RatNum.java" >| ${INST_LIST_FILE}

# Don't check RatNum, as we already have in another e.g.
ESC_TO_CHECK:=merged/PolyCalc/RatPoly.java merged/PolyCalc/RatTermVec.java merged/PolyCalc/RatTerm.java

restrict-%:
	-rm -f ${PROJECT_FILE}.inv
	-mv -f ${SCRATCH_FOR_DAIKON}/${PROJECT_FILE}.dtrace.bak ${SCRATCH_FOR_DAIKON}/${PROJECT_FILE}.dtrace
	-mv -f ${PROJECT_FILE}.spinfo.bak ${PROJECT_FILE}.spinfo
	trace-purge-fns.pl -v "\b$*[:\.]" ${SCRATCH_FOR_DAIKON}/${PROJECT_FILE}.dtrace
	trace-purge-fns.pl -v "\b$*[:\.]" ${PROJECT_FILE}.spinfo
