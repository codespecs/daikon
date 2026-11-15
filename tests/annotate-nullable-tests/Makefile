# Tests of AnnotateNullable on stored traces.
#
# For each test name T in TESTS, Daikon reads T.decls and T.dtrace.gz, and
# AnnotateNullable writes an annotation file T.txt-jaif from Daikon's output.
# Test T passes if T.txt-jaif is the same as T.txt-jaif.goal.
#
# Test "PointChildren":  the CLASS and OBJECT program points each have a child
# that is a point, not a method.

TESTS = PointChildren

# The directory containing this file.  Makefile.jdk is found relative to it,
# not via DAIKONDIR, which can name a different Daikon tree.
THIS_MAKEFILE_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
# DAIKONDIR is the clone of the 'daikon' project.
ifeq ($(origin DAIKONDIR), undefined)
  DAIKONDIR := $(realpath ${THIS_MAKEFILE_DIR}../..)
endif
include ${THIS_MAKEFILE_DIR}../Makefile.jdk

# A prerequisite that changes whenever Daikon is rebuilt: Daikon.class if the
# java/ tree is compiled, otherwise daikon.jar.
DAIKON := $(firstword $(wildcard ${DAIKONDIR}/java/daikon/Daikon.class ${DAIKONDIR}/daikon.jar))
ifeq (${DAIKON},)
  ifneq ($(filter-out clean update-goals,$(or ${MAKECMDGOALS},diffs)),)
    $(error Daikon is not built.  Run "make -C ${DAIKONDIR} compile")
  endif
endif

DAIKON_CLASSPATH := ${DAIKONDIR}/java:${DAIKONDIR}/java/lib/*:${DAIKONDIR}/daikon.jar
# The command that runs a Daikon tool; its first argument is the tool's class.
DAIKON_JAVA = ${JAVA} -cp ${DAIKON_CLASSPATH} -ea -Xmx7g

# If a recipe fails, delete its target, so that a partially written output
# file does not make the next run appear up to date.
.DELETE_ON_ERROR:

.PHONY: all test diffs update-goals clean
all test: diffs

# For each test T, a failing diff is left in T.txt-jaif-failed.diff for inspection.
diffs: $(addsuffix .txt-jaif.diff,${TESTS})
%.txt-jaif.diff: %.txt-jaif.goal %.txt-jaif
	diff -u $^ > $*.txt-jaif-failed.diff || (cat $*.txt-jaif-failed.diff && false)
	mv $*.txt-jaif-failed.diff $@

# Keep the annotation files for inspection after a test runs.
.SECONDARY: $(addsuffix .txt-jaif,${TESTS})

%.txt-jaif: %.inv.gz ${DAIKON}
	${DAIKON_JAVA} daikon.AnnotateNullable $< > $@

%.inv.gz: %.decls %.dtrace.gz ${DAIKON}
	${DAIKON_JAVA} daikon.Daikon --no_text_output --no_show_progress \
	  --config ${DAIKONDIR}/java/daikon/annotate_nullable.config \
	  --config_option daikon.Debug.show_stack_trace=true \
	  -o $@ $*.decls $*.dtrace.gz

update-goals: $(addsuffix .txt-jaif,${TESTS})
	for t in ${TESTS}; do cp -pf $$t.txt-jaif $$t.txt-jaif.goal; done

clean:
	/bin/rm -f *.inv.gz *.txt-jaif *.diff
