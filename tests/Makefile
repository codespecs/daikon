###########################################################################

# Read Makefile.common for what all of this means

# help listing, split up so that children can use some of it
default help: help-here help-recurse-list
help-here:
	@echo "make options for all tests"
	@echo "  make quick        : run subset of regression tests and display results"
	@echo ""
	@echo "  make all          : run all regression tests (except Kvasir), display results"
	@echo "  make diffs        : same as 'make all'"
	@echo "  make results      : don't run tests, just display last results"
	@echo "  make summary      : don't run tests, just display summary of last results"
help-recurse: help-recurse-top help-recurse-list
help-recurse-top:
	@echo "make options for this directory"
help-recurse-list:
	@echo "  make clean        : removes all data from prior runs of the tests"
	@echo "  make txt-diff     : computes diffs between current output and goals"
	@echo "  make update-goals : resets the goals to be the current output"
	@echo "  make txt-daikon   : runs only daikon itself and none of the post processing tools"
	@echo ""
	@echo "(Each of these options processes only the active subdirectories,"
	@echo " which are ones that have been selected as useful tests.)"
	@echo ""

# To produce incremental output, invoke make as: make MPARG=-Otarget ...
# To not run in parallel, invoke make as: make MPARG=-j1 ...
FIND_VERSION := $(shell make --version 2>&1 | head -1)
ifeq (4.,$(findstring 4.,$(FIND_VERSION)))
  MPARG ?= -Orecurse
else
  MPARG ?=
endif

all diffs: txt-diff non-txt-diff dyncomp-tests results

# These do not use DynComp
non-txt-diff: diff-tests chicory-tests merge-invariants-tests parser-tests


quick: quick-txt-diff diff-tests results

# targets that descend to child direcories
quick-txt-diff nonquick-txt-diff txt-diff update-goals merge spinfo-static txt-suppress inv-checker-diff \
txt-daikon txt-chicory cross-checker chicory chicory-diff \
esc jml inv txt-daikon-diff txt-esc-diff txt-jml-diff txt-csharpcontract-diff txt-merge-diff \
txt-chicory-diff everything-txt-diff stats dyncomp-diff txt-jaif-diff txt-jaif:
	$(MAKE) pat-daikon-tests-$@
pat-daikon-tests-%:
	$(MAKE) -C daikon-tests --jobs=4 $(MPARG) $*

list-targets:
	$(MAKE) -C daikon-tests list-targets

diff-tests:
	$(MAKE) -C diff-tests
.PHONY: diff-tests

chicory-tests:
	$(MAKE) -C chicory-tests
.PHONY: chicory-tests

merge-invariants-tests:
	$(MAKE) -C merge-invariants-tests
.PHONY: merge-invariants-tests

parser-tests:
	$(MAKE) -C parser-tests
.PHONY: parser-tests

dyncomp-tests:
	$(MAKE) -C dyncomp-tests regression-tests
.PHONY: dyncomp-tests

clean:
	$(MAKE) -C chicory-tests clean
	$(MAKE) -C daikon-tests clean
	$(MAKE) -C diff-tests clean
	$(MAKE) -C merge-invariants-tests clean
	$(MAKE) -C parser-tests clean
	$(MAKE) -C dyncomp-tests clean

very-clean: clean
	$(MAKE) -C kvasir-tests clean-all

# output display; can be called from children (they will set BASE)
BASE?=chicory-tests daikon-tests diff-tests merge-invariants-tests parser-tests dyncomp-tests
# removes fields before the size (ie, permissions, owner, group).
PERL_CLEANUP_LS_OUTPUT = perl -ne 'BEGIN { $$failure=0; } /^\S+\s+\S+\s+\S+\s+\S+\s+(\d+)\s+(.*)\s+(\S+)$$/; print "$$1\t$$2\t$$3\n"; if ($$1 > 0) { $$failure++; } END { if ($$failure == 1) { print "1 test failed.\n"; exit 1; } elsif ($$failure) { print "$$failure tests failed.\n"; exit 1; } else { print "All tests succeeded.\n"; } }'
# args to 'find' program, to find files containg results
RESULTS_PATTERN :=    -name '*.txt-*.diff' \
                   -o -name '*.checker.diff' \
                   -o -name '*.spinfo-static.diff' \
                   -o -name '*.txt-*-failed' \
                   -o -name '*.txt.diff' \
                   -o -name '*.decls-DynComp.diff' \
                   -o -name '*.decls-DynComp-nojdk.diff' \
                   -o -name '*.txt-cset-nojdk.diff' \
                   -o -name '*.java-parsed.diff' \
                   -o -name '*.comp.diff' \
                   -o -name '*.counts.diff' \
                   -o -name '*.dtrace.diff' \
                   -o -name '*.invs.diff' \
                   -o -name '*xcheck.diff' \
                   -o -name '*daikon.diff'
results_header:
	@echo ""
	@echo "=== RESULTS ==="
	@echo ""

results_cat:
	@find ${BASE} $(RESULTS_PATTERN) | xargs cat

results: results_header results_cat summary

summary:
	@ls -l `find ${BASE} $(RESULTS_PATTERN)` \
	  | perl -pe 's|\Q${BASE}|.|;' \
	  | ${PERL_CLEANUP_LS_OUTPUT}

summary2: # omit pristine tests
	@$(MAKE) --no-print-directory summary | egrep -v '^0' || echo 'No differences'

summary3: # also omit esc differences
	@$(MAKE) --no-print-directory summary2 | grep -v 'txt-esc' || echo 'No non-ESC differences'
