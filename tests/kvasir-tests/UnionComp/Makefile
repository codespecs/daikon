NAME := UnionComp
# This test contains a union of a char* and ptr*.
# The ptr (next) is assigned an address giving the char* (name) random garbage.
# Hence, the .dtrace file will almost never match.
SKIP_DTRACE_DIFF := 1
SKIP_INVS_DIFF := 1

include ../Makefile.common
