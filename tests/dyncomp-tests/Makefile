# Makefile.common contains the targets for running an individual test.
# This Makefile is a driver for running all the tests.

# HOW TO ADD A FILE TO THE REGRESSION TESTSUITE:
# 1. Create a directory for the test (myprog).
# 2. Put the source in the directory (myprog.c).
# 3. Make a Makefile in the directory that sets NAME to myprog and
#    includes ../Makefile.common (for more kinds of customization,
#    look at the other directories' Makefiles).
# 4. Create myprog.{decls,dtrace,out}.goal files in the directory. It's
#    OK if they start out empty; you can update them later with
#    "make update-goals".
#
# In this file:
# 5. Put the test into one of the existing categories, or make a new
#    category to contain it.
#

# Forked from $DAIKONDIR/tests/kvasir-tests/Makefile

default: summary

.PHONY: regression-tests
regression-tests:
	${MAKE} nightly-tests__diffs-w-daikon
	${MAKE} nightly-summary-only-w-daikon
	@${MAKE} --no-print-directory results

summary: default-projects__diffs
	${MAKE} summary-only

summary-w-daikon: default-projects__diffs-w-daikon
	${MAKE} summary-only-w-daikon

summary-only: default-projects__summary__quietly

summary-only-w-daikon: default-projects__summary-w-daikon__quietly

# This target simulates what the nightly build-test does, but note that
# it actually makes the two targets separately (so that it can skip the
# second if Daikon didn't build).
nightly: nightly-summary nightly-summary-w-daikon

nightly-summary: nightly-tests__diffs
	${MAKE} nightly-summary-only

nightly-summary-w-daikon: nightly-tests__diffs-w-daikon
	${MAKE} summary-only-w-daikon

nightly-summary-only: nightly-tests__summary__quietly

nightly-summary-only-w-daikon: nightly-tests__summary-w-daikon__quietly

clean diffs invs update-goals:
	${MAKE} default-projects__$@

nightly-tests__%: default-projects__%
	@

update-all:
	${MAKE} nightly-tests__update-goals

.PHONY: clean-all
clean-all:
	${MAKE} --jobs=4 ${MPARG} nightly-tests__clean


default-projects__%: pgbovine-tests__%
	@

pgbovine-tests__%: do-Apples__% do-DerivedComparability__% do-FloatTest__% \
                   do-FunctionParamsTest__% do-IntFromFloatTest__% do-Logicals__% \
		   do-PointerComp__% do-PricesTest__% do-WhatIsInteraction__% \
		   do-SimpleStructTest__% do-PrintfTest__% do-StrcpyTest__% \
		   do-GlibcTest__% do-LocalVarTest__%
	@

# 'make do-SomeDiectory_target'
# e.g. 'make do-bzip2__clean'
do-%__quietly:
	@${MAKE} --jobs=1 --no-print-directory -C `echo $* | perl -pe 's/(.+?)__/$$1 /;'`

do-%:
	@${MAKE} --jobs=1 -C `echo $* | perl -pe 's/(.+?)__/$$1 /;'`

# removes fields before the size (ie, permissions, owner, group).
PERL_CLEANUP_LS_OUTPUT = perl -ne 'BEGIN { $$failure=0; } /^\S+\s+\S+\s+\S+\s+\S+\s+(\d+)\s+(.*)\s+(\S+)$$/; if ($$1 > 0) { $$failure++; print "$$1\t$$2\t$$3\n";} END { if ($$failure == 1) { print "1 test failed.\n"; exit 1; } elsif ($$failure) { print "$$failure tests failed.\n"; exit 1; } else { print "All tests succeeded.\n"; } }'
# args to 'find' program, to find files containg results
RESULTS_PATTERN :=    -name '*.diff'

results_header:
	@echo ""
	@echo "=== RESULTS ==="

results_cat:
	@find . ${RESULTS_PATTERN} | xargs cat

results: results_header results_cat summary_list

summary_list:
	@ls -l `find . ${RESULTS_PATTERN}` \
	  | perl -pe 's|\Q${BASE}|.|;' \
	  | ${PERL_CLEANUP_LS_OUTPUT}
