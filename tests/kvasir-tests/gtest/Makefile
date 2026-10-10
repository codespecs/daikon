NAME := gtest
CUSTOM_BUILD := 1
# dtrace file contains timing variables and is very large
SKIP_DTRACE_DIFF := 1
# inv file contains timing variables and current directory dependencies
SKIP_INVS_DIFF := 1
# counts file contains a current directory dependency
REMOVE_VARIABLE_COUNTS := 1

include ../Makefile.common

gtest: sample1_unittest
	cp sample1_unittest gtest
