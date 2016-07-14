NAME := ConstMergeTest
CPLUSPLUS := 1
C_EXT := cpp
CUSTOM_BUILD := 1
OBJS = file2.o \
       ConstMergeTest.o 
EXTRA_JUNK = $(OBJS) ConstMergeTest

include ../Makefile.common

ConstMergeTest: $(OBJS)
	$(CXX) $(CFLAGS) $(LDFLAGS) -o ConstMergeTest $(OBJS)

ConstMergeTest.o:
	$(CXX) $(CFLAGS)  -c ConstMergeTest.cpp -o ConstMergeTest.o

file2.o:
	$(CXX) $(CFLAGS) -gdwarf-2 -c file2.cpp -o file2.o
