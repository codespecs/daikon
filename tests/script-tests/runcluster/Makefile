# Tests for runcluster.pl and the scripts that it calls.
# "make" runs all the tests and fails if any of them fails.

DAIKONDIR ?= $(realpath ../../..)
SCRIPTDIR := ${DAIKONDIR}/scripts
DAIKON_JAR := ${DAIKONDIR}/daikon.jar

# runcluster.pl calls the kmeans executable, which must be on the path.
export PATH := ${SCRIPTDIR}:${PATH}

.PHONY: test
test: test-good test-newdecls test-duplicate-decls test-merge-stems
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
# Daikon can use the splitter info file.
	cd work-good && java -cp ${DAIKON_JAR} daikon.Daikon --no_show_progress cluster-km-2.spinfo good.decls good.dtrace > daikon-output.txt
	grep -F 'Sensor.read():::POINT: 4 of 4 splitters successful' work-good/daikon-output.txt

# A dtrace file may contain the declarations.
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

# Each exit point of a method has its own cluster file.  dtrace-add-cluster.pl
# clusters the samples of every exit point, and it gives the clusters of
# different exit points different numbers.
MERGE_STEMS_CLUSTER_FILES := Foo.barint.EXIT10.runcluster_temp.cluster Foo.barint.EXIT20.runcluster_temp.cluster
.PHONY: test-merge-stems
test-merge-stems:
	rm -rf work-merge-stems
	mkdir work-merge-stems
	cp merge-stems.dtrace work-merge-stems
	cp merge-stems-EXIT10.cluster work-merge-stems/Foo.barint.EXIT10.runcluster_temp.cluster
	cp merge-stems-EXIT20.cluster work-merge-stems/Foo.barint.EXIT20.runcluster_temp.cluster
	cd work-merge-stems && ${SCRIPTDIR}/dtrace-add-cluster.pl -a km merge-stems.dtrace ${MERGE_STEMS_CLUSTER_FILES}
	diff -u merge-stems_runcluster_temp.dtrace.goal work-merge-stems/merge-stems_runcluster_temp.dtrace
	test "$$(cat work-merge-stems/runcluster_temp.maxcluster)" = 4

.PHONY: clean
clean:
	rm -rf work-good work-newdecls work-duplicate-decls work-merge-stems
