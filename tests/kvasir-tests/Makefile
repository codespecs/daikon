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

include $(dir $(lastword $(MAKEFILE_LIST)))../Makefile.jobs

default: summary-w-daikon

# Variables used by Makefile.results.
REGRESSION_TESTS_ARGS = --jobs=${DAIKON_TEST_JOBS} ${MPARG}
REGRESSION_TESTS_TARGET = nightly-tests__diffs-w-daikon-if-working
REGRESSION_SUMMARY_TARGET = nightly-report-w-daikon

summary: default-projects__diffs
	${MAKE} summary-only

summary-w-daikon: default-projects__diffs-w-daikon
	${MAKE} summary-only-w-daikon

working: default-projects__diffs-if-working
	${MAKE} working-summary-only

working-w-daikon: default-projects__diffs-w-daikon-if-working
	${MAKE} working-summary-only-w-daikon

summary-only: default-projects__summary__quietly

summary-only-w-daikon: default-projects__summary-w-daikon__quietly

working-summary-only: default-projects__summary-if-working__quietly

working-summary-only-w-daikon: default-projects__summary-w-daikon-if-working__quietly

# This target simulates what the nightly build-test does, but note that
# it actually makes the two targets separately (so that it can skip the
# second if Daikon didn't build).
nightly: nightly-summary nightly-summary-w-daikon

nightly-summary: nightly-tests__diffs-if-working
	${MAKE} nightly-summary-only

nightly-summary-w-daikon: nightly-tests__diffs-w-daikon-if-working
	${MAKE} nightly-summary-only-w-daikon

nightly-summary-only: nightly-tests__summary-if-working__quietly

nightly-summary-only-w-daikon: nightly-tests__summary-w-daikon-if-working__quietly

# Like nightly-summary-only-w-daikon, but runs no tests.
nightly-report-w-daikon: nightly-tests__report-w-daikon__quietly

clean diffs invs update-goals:
	${MAKE} default-projects__$@

nightly-tests__%: default-projects__% medium-tests__%
	@

update-all:
	${MAKE} nightly-tests__update-goals

.PHONY: clean-all
# Cleaning uses little memory, and it must work even if plume-scripts has not
# been cloned, so it does not use DAIKON_TEST_JOBS.
clean-all:
	${MAKE} --jobs=4 ${MPARG} nightly-tests__clean

default-projects__%: pgbovine-tests__% smcc-tests__% tiny-real-tests__% tws-tests__%
	@

# This target is not used in standard test runs.
# The tests are duplicated in the pgbovine-tests target.
cpp-tests__%: do-CppFunctionTest__% do-SimpleClassesTest__% \
              do-Dstr__% do-CppInlineTest__% do-ArrayTest-cpp__% \
              do-StackCppTest__%  do-StackCppTestInline__% do-StackArrayCppTest__%
	@

medium-tests__%: rothermel-tests__% medium-real-tests__% medium-crypto-tests__% \
                 missing-tests__% small-crypto-tests__% do-space__% do-gtest__% do-Dstr__% do-partial-init__%
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

missing-tests__%: do-ConstantTest__% do-ConstMergeTest__% do-ExceptionTest__% \
		   do-pthread-test__% do-rdtsc__% do-setjmp-test__% \
		   do-shared-lib__% do-sigaltstack-test__% do-UnionComp__% \
		   do-virtual-method__%
	@

tws-tests__%: do-SelectGlobalsTest__%
	@

smcc-tests__%: do-function-pointer__% do-two-statics__% do-pointer-levels__% \
               do-string-arrays__% do-static-struct__% do-inline-func__% \
		       do-printf-interact__% do-iostream-interact__% do-local-ptr__% \
		       do-dtrace-append__%
	@

rothermel-tests__%: do-flex-rothermel__%
	@

tiny-real-tests__%: do-wordplay__% do-bc__%
	@

# omit povray test; see povray/README for details
#medium-real-tests__%: do-gzip__% do-flex-pristine__% do-bzip2__% do-perl__%  do-povray__%  do-tcas__%
medium-real-tests__%: do-gzip__% do-flex-pristine__% do-bzip2__% do-perl__%   do-tcas__%
	@

small-crypto-tests__%: do-md5__%
	@

medium-crypto-tests__%: do-rijndael__%
	@

# 'make do-SomeDiectory_target'
# e.g. 'make do-bzip2__clean'
do-%__quietly:
	@${MAKE} --jobs=1 --no-print-directory -C `echo $* | perl -pe 's/(.+?)__/$$1 /;'`

# --no-keep-going: within one test directory, stop at the first error, even
# under "make -k" (which the regression-tests target uses).
do-%:
	@${MAKE} --jobs=1 --no-keep-going -C `echo $* | perl -pe 's/(.+?)__/$$1 /;'`

include ${DAIKON_TESTS_DIR}Makefile.results
