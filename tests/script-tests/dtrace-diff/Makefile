# Tests for dtrace-diff.pl.
# "make" runs all the tests and fails if any of them fails.

SCRIPTDIR := $(realpath ../../../scripts)

.PHONY: test
test: test-keyword-ppt-names-same test-keyword-ppt-names-different
	@echo "All dtrace-diff tests passed."

# A sample whose program point name starts with a header keyword, such as
# "input-language:::POINT", is a sample, not a header.
.PHONY: test-keyword-ppt-names-same
test-keyword-ppt-names-same:
	${SCRIPTDIR}/dtrace-diff.pl keyword-ppt-names.dtrace keyword-ppt-names.dtrace

.PHONY: test-keyword-ppt-names-different
test-keyword-ppt-names-different:
	! ${SCRIPTDIR}/dtrace-diff.pl keyword-ppt-names.dtrace keyword-ppt-names-2.dtrace > keyword-ppt-names-different.txt
	cat keyword-ppt-names-different.txt
	grep -F 'x @ input-language:::POINT difference:' keyword-ppt-names-different.txt
	rm -f keyword-ppt-names-different.txt

.PHONY: clean
clean:
	rm -f keyword-ppt-names-different.txt
