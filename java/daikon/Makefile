default:
	${MAKE} -C .. default

.PHONY: chicory
chicory:
	${MAKE} -C .. chicory

.PHONY: dcomp
dcomp:
	${MAKE} -C .. dcomp

.PHONY: quick-test
quick-test:
	${MAKE} -C ../.. quick-test

%:
	${MAKE} -C .. $@

.PHONY: all test clean
all:
	${MAKE} -C .. all
test:
	${MAKE} -C .. test
clean:
	${MAKE} -C .. clean
