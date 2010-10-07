###########################################################################

# Read Makefile.common for what all of this means

MAIN_CLASS:=six170.HuffmanMain

include ../../Makefile.common

instrument-files-revise:
	mv ${INST_LIST_FILE} ${INST_LIST_FILE}.old
	perl -ne 'print unless /Main/;' ${INST_LIST_FILE}.old > ${INST_LIST_FILE}
