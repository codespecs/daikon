# Tests for runcluster.pl and the scripts that it calls.
# "make" runs all the tests and fails if any of them fails.

DAIKONDIR ?= $(realpath ../../..)
SCRIPTDIR := ${DAIKONDIR}/scripts
DAIKON_JAR := ${DAIKONDIR}/daikon.jar

# runcluster.pl calls the kmeans executable, which must be on the path.
export PATH := ${SCRIPTDIR}:${PATH}

.PHONY: test
test: test-good test-newdecls test-duplicate-decls test-malformed test-malformed-newdecls test-daikon-bad-option
	@echo "All runcluster tests passed."

# runcluster.pl writes its temporary files to the current directory, so run
# each test in its own directory.

# On a well-formed trace, runcluster.pl produces a splitter info file.
.PHONY: test-good
test-good:
	rm -rf work-good
	mkdir work-good
	cp good.decls good.dtrace work-good
	cd work-good && ${SCRIPTDIR}/runcluster.pl -a km -k 2 good.dtrace good.decls
	diff -u good-km-2.spinfo.goal work-good/cluster-km-2.spinfo

# A dtrace file may contain declarations, in the new (version 2.0) format.
.PHONY: test-newdecls
test-newdecls:
	rm -rf work-newdecls
	mkdir work-newdecls
	cp good-newdecls.dtrace work-newdecls
	cd work-newdecls && ${SCRIPTDIR}/runcluster.pl -a km -k 2 good-newdecls.dtrace
	diff -u good-km-2.spinfo.goal work-newdecls/cluster-km-2.spinfo

# A program point may be declared in more than one decls file.
.PHONY: test-duplicate-decls
test-duplicate-decls:
	rm -rf work-duplicate-decls
	mkdir work-duplicate-decls
	cp good.decls good.dtrace work-duplicate-decls
	cp good.decls work-duplicate-decls/good2.decls
	cd work-duplicate-decls && ${SCRIPTDIR}/runcluster.pl -a km -k 2 good.dtrace good.decls good2.decls
	diff -u good-km-2.spinfo.goal work-duplicate-decls/cluster-km-2.spinfo

# On a trace whose records omit a variable, runcluster.pl fails with a message
# that names the problem.
.PHONY: test-malformed
test-malformed:
	rm -rf work-malformed
	mkdir work-malformed
	cp malformed.decls malformed.dtrace work-malformed
	cd work-malformed && ! ${SCRIPTDIR}/runcluster.pl -a km -k 2 malformed.dtrace malformed.decls 2> stderr.txt
	cat work-malformed/stderr.txt
	grep -F 'malformed_runcluster_temp_nonces.dtrace line 4: expected variable time, got 0.100 for program point Sensor.read():::POINT' work-malformed/stderr.txt
	grep -F 'malformed_runcluster_temp_nonces.dtrace is a copy of malformed.dtrace' work-malformed/stderr.txt

# The same, with declarations in the new format, in the dtrace file.
.PHONY: test-malformed-newdecls
test-malformed-newdecls:
	rm -rf work-malformed-newdecls
	mkdir work-malformed-newdecls
	cp malformed-newdecls.dtrace work-malformed-newdecls
	cd work-malformed-newdecls && ! ${SCRIPTDIR}/runcluster.pl -a km -k 2 malformed-newdecls.dtrace 2> stderr.txt
	cat work-malformed-newdecls/stderr.txt
	grep -F 'expected variable time, got 0.100 for program point Sensor.read():::POINT' work-malformed-newdecls/stderr.txt

# Daikon exits with a non-zero status when given an unrecognized option, so
# that scripts such as runcluster.pl can detect the failure.
.PHONY: test-daikon-bad-option
test-daikon-bad-option:
	! java -cp ${DAIKON_JAR} daikon.Daikon --no-such-option > /dev/null

.PHONY: clean
clean:
	rm -rf work-good work-newdecls work-duplicate-decls work-malformed work-malformed-newdecls
