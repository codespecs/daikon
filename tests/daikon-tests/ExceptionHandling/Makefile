###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:= ExceptionHandling.ExceptionHTest
THISPWD:=$(shell pwd)

CHICORY_USER_FLAGS:= --exception-handling --purity-file=${THISPWD}/DaikonAnalyse.pure

include ../../Makefile.common

instrument-files-revise:
	echo "ExceptionHandling/DaikonAnalyseExceptions.java" >| ${INST_LIST_FILE}
