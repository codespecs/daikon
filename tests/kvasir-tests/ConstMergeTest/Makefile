NAME := ConstMergeTest
CPLUSPLUS := 1
C_EXT := cpp
CUSTOM_BUILD := 1
OBJS = file2.o \
       ConstMergeTest.o 
EXTRA_JUNK = ${OBJS} ConstMergeTest

include ../Makefile.common

ConstMergeTest: ${OBJS}
	${CXX} ${CFLAGS} ${LDFLAGS} -o  $@ $^

ConstMergeTest.o: ConstMergeTest.cpp
	${CXX} ${CFLAGS}  -c $< -o $@

file2.o: file2.cpp
	${CXX} ${CFLAGS} -gdwarf-2 -c $< -o $@
