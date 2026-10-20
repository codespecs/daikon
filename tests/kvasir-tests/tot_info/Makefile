NAME := tot_info
TESTS_SCRIPT := tests.sh
# tot_info exits with status 1 on its test input.
EXPECTED_EXIT_STATUS := 1
AFTER_RUN := perl -i ../fix-decls.pl daikon-output/tot_info.dtrace
LIBS := -lm

include ../Makefile.common
