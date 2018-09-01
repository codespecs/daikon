# Makefile.common contains the targets for running an individual test.
# This Makefile is a driver for running all the tests.

# HOW TO ADD A DIRECTORY TO THE DIFF REGRESSION TESTS

# 1. Create a directory for the program (myprog).

# 2. Put the following files in the directory: myprog.decls,
# myprog-1.dtrace.gz, myprog-2.dtrace.gz.  Copy the Makefile
# from an existing test directory into the new directory.

# 3. Run 'make update-goals', and examine the goal files to make sure
# the output is correct.

# 4. Run 'make clean; make', and make sure all the tests are passed.

# 5. Run 'make clean'.

# 6. Check the directory and remaining files into the repository.

# XXX HACK:  must remove "grep -v StackAr"
SUBDIRS = $(shell find * -maxdepth 0 -type d | grep -v StackAr)

default:

# The regression tests seem to break if the "@" is omitted from this line.
%:
	@for dir in $(SUBDIRS); \
	  do echo "Entering diff-tests subdirectory: $$dir"; \
	     $(MAKE) -C $$dir $*; \
	done

showvars:
	echo "SUBDIRS = ${SUBDIRS}"
	echo "CLASSPATH = $(CLASSPATH)"
	echo "DAIKON_CLASSPATH = $(DAIKON_CLASSPATH)"
