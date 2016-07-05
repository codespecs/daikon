# Makefile.common contains the targets for running an individual test.
# This Makefile is a driver for running all the tests.

# HOW TO ADD A FILE TO THE REGRESSION TESTSUITE:
# 1. Run make-test.pl $TEST_NAME
# 2. Edit $TEST_NAME/$TEST_NAME.c and edit the Makefile
#    as necessary
# 3. The test suite can be run with "make". When the results
#    are correct, you can update the goals with "make update-goals"
#
# In this file:
# 4. Put the test into one of the existing categories, or make a new
#    category to contain it.
#

FIND_VERSION := $(shell make --version 2>&1 | head -1)
ifeq (4.,$(findstring 4.,$(FIND_VERSION)))
  MPARG := -Orecurse
else
  MPARG :=
endif

default: summary

.PHONY: regression-tests
regression-tests:
	$(MAKE) --jobs=4 $(MPARG) nightly-tests__diffs-w-daikon-if-working
	$(MAKE) nightly-summary-only-w-daikon
	@$(MAKE) --no-print-directory results

.PHONY: TEMP-regression-tests
TEMP-regression-tests:
	$(MAKE) --jobs=4 $(MPARG) TEMP-nightly-tests__diffs-w-daikon-if-working
	$(MAKE) TEMP-nightly-summary-only-w-daikon
	@$(MAKE) --no-print-directory results

summary: default-projects__diffs
	$(MAKE) summary-only

summary-w-daikon: default-projects__diffs-w-daikon
	$(MAKE) summary-only-w-daikon

working: default-projects__diffs-if-working
	$(MAKE) working-summary-only

working-w-daikon: default-projects__diffs-w-daikon-if-working
	$(MAKE) working-summary-only-w-daikon

summary-only: default-projects__summary__quietly

summary-only-w-daikon: default-projects__summary-w-daikon__quietly

working-summary-only: default-projects__summary-if-working__quietly

working-summary-only-w-daikon: default-projects__summary-w-daikon-if-working__quietly

# This target simulates what the nightly build-test does, but note that
# it actually makes the two targets separately (so that it can skip the
# second if Daikon didn't build).
nightly: nightly-summary nightly-summary-w-daikon

nightly-summary: nightly-tests__diffs-if-working
	$(MAKE) nightly-summary-only

nightly-summary-w-daikon: nightly-tests__diffs-w-daikon-if-working
	$(MAKE) nightly-summary-only-w-daikon

TEMP-nightly-summary-w-daikon: TEMP-nightly-tests__diffs-w-daikon-if-working
	$(MAKE) TEMP-nightly-summary-only-w-daikon

nightly-summary-only: nightly-tests__summary-if-working__quietly

nightly-summary-only-w-daikon: nightly-tests__summary-w-daikon-if-working__quietly

TEMP-nightly-summary-only-w-daikon: TEMP-nightly-tests__summary-w-daikon-if-working__quietly

clean diffs invs update-goals:
	$(MAKE) default-projects__$@

# nightly-tests__%: default-projects__% medium-tests__%
#	@
nightly-tests__%: default-projects__% medium-easy-tests__%
	@
TEMP-nightly-tests__%: default-projects__% TEMP-medium-easy-tests__%
	@

update-all:
	$(MAKE) nightly-tests__update-goals

.PHONY: clean-all
clean-all:
	$(MAKE) --jobs=4 $(MPARG) nightly-tests__clean


default-projects__%: pgbovine-tests__% smcc-tests__% tiny-real-tests__% tws-tests__%
	@

# This target is not used in standard test runs.
# The tests are duplicated in the pgbovine-tests target.
cpp-tests__%: do-CppFunctionTest__% do-SimpleClassesTest__% \
              do-Dstr__% do-CppInlineTest__% do-ArrayTest-cpp__% \
              do-StackCppTest__%  do-StackCppTestInline__% do-StackArrayCppTest__%
	@

medium-tests__%: rothermel-tests__% medium-real-tests__% medium-crypto-tests__% \
                 missing-tests__% small-crypto-tests__% do-space__% test-gtest__% do-Dstr__% do-partial-init__%
	@

# Omits povray and perl
medium-easy-tests__%: rothermel-tests__% medium-real-easy-tests__% medium-crypto-tests__% \
                 missing-tests__% small-crypto-tests__% do-space__% test-gtest__% do-Dstr__% do-partial-init__%
	@

# Omits povray and perl
# Omits flex-pristine and bzip2
# Omits flex-rothermel, md5 and space
TEMP-medium-easy-tests__%: TEMP-medium-real-easy-tests__% \
                 missing-tests__%
	@

pgbovine-tests__%: do-IntTest__% do-PointerTest__% do-TrivialTest__% \
		   do-TypesTest__% do-crazy-test-1__% do-ArrayTest__% \
		   do-GlobalTest__% do-NestedStructTest__% \
		   do-StaticArraysTest__% do-StructPtrTest__% do-TypedefTest__% \
		   do-small-test__% do-DisambigTest__% do-SimpleDisambigStructTest__% \
		   do-StackCppTest__%  do-StackCppTestInline__% do-StackArrayCppTest__% \
		   do-MultiDimArrayTest__% do-ArraysInStructTest__% \
		   do-FunctionNamesTest__% do-CoercePointerTest__% \
		   do-CppFunctionTest__% do-SimpleClassesTest__% \
		   do-CppInlineTest__% do-ArrayTest-cpp__% do-FloatTest__% \
		   do-print_tokens__% do-print_tokens2__% do-replace__% do-tot_info__% \
		   do-NestedStructVarList__%
	@

ifeq (x86_64,$(shell uname -m))
ifeq (tern,$(findstring tern,$(shell uname -n)))
# gtest won't run on tern do to old software
test-gtest__%:
	@
else
test-gtest__%: do-gtest__%
	@
endif
else
# gtest is too big to run on 32 bits
test-gtest__%:
	@
endif

ifeq (tern,$(findstring tern,$(shell uname -n)))
# bc won't run on tern do to old software
test-bc__%:
	@
else
test-bc__%: do-bc__%
	@
endif

ifeq (tern,$(findstring tern,$(shell uname -n)))
# gzip won't run on tern do to old software
test-gzip__%:
	@
else
test-gzip__%: do-gzip__%
	@
endif

missing-tests__%: do-ConstantTest__% do-ConstMergeTest__% do-ExceptionTest__% \
		   do-pthread-test__% do-rdtsc__% do-setjmp-test__% \
		   do-shared-lib__%  do-UnionComp__% do-virtual-method__%
	@

tws-tests__%: do-SelectGlobalsTest__%
	@

smcc-tests__%: do-function-pointer__% do-two-statics__% do-pointer-levels__% \
               do-string-arrays__% do-static-struct__% do-inline-func__% \
	       do-printf-interact__% do-iostream-interact__% do-local-ptr__% \
               do-dtrace-append__%
	@

decls-2-compile: pgbovine-tests__decls-2-compile smcc-tests__decls-2-compile medium-tests__decls-2-compile
	$(MAKE) decls-2-compile

decls-2-large:
	$(MAKE) decls-2-large

rothermel-tests__%: do-flex-rothermel__%
	@

tiny-real-tests__%: do-wordplay__% test-bc__%
	@

small-real-tests__%:
	@

medium-real-tests__%: test-gzip__% do-flex-pristine__% do-bzip2__% do-perl__%  do-povray__% \
                      do-tcas__%
	@

# Omits povray and perl
medium-real-easy-tests__%: test-gzip__% do-flex-pristine__% do-bzip2__% do-tcas__%
	@

# Omits povray and perl
# Omits flex-pristine and bzip2
TEMP-medium-real-easy-tests__%: test-gzip__% do-tcas__%
	@

small-crypto-tests__%: do-md5__%
	@

medium-crypto-tests__%: do-rijndael__%
	@

# 'make do-SomeDiectory_target'
# e.g. 'make do-bzip2__clean'
do-%__quietly:
	@$(MAKE) --jobs=1 --no-print-directory -C `echo $* | perl -pe 's/(.+?)__/$$1 /;'`

do-%:
	@$(MAKE) --jobs=1 -C `echo $* | perl -pe 's/(.+?)__/$$1 /;'`

# removes fields before the size (ie, permissions, owner, group).
PERL_CLEANUP_LS_OUTPUT = perl -ne 'BEGIN { $$failure=0; } /^\S+\s+\S+\s+\S+\s+\S+\s+(\d+)\s+(.*)\s+(\S+)$$/; if ($$1 > 0) { $$failure++; print "$$1\t$$2\t$$3\n";} END { if ($$failure == 1) { print "1 test failed.\n"; exit 1; } elsif ($$failure) { print "$$failure tests failed.\n"; exit 1; } else { print "All tests succeeded.\n"; } }'
# args to 'find' program, to find files containg results
RESULTS_PATTERN :=    -name '*.diff'

results_header:
	@echo ""
	@echo "=== RESULTS ==="

results_cat:
	@find . $(RESULTS_PATTERN) | xargs cat

results: results_header results_cat summary_list

summary_list:
	@ls -l `find . $(RESULTS_PATTERN)` \
	  | perl -pe 's|\Q${BASE}|.|;' \
	  | ${PERL_CLEANUP_LS_OUTPUT}
