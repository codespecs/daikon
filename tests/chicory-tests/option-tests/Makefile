#
# Makefile that runs tests that checks various chicory options.
#

# DAIKONDIR is the clone of the 'daikon' project.
DAIKONDIR := $(realpath $(dir $(lastword ${MAKEFILE_LIST}))../../..)

# StackAr sources are used for option testing
SRC_DIR = ${DAIKONDIR}/tests/sources
SRC_FILES = DataStructures/Comparable.java \
			DataStructures/Hashable.java \
			DataStructures/MyInteger.java \
			DataStructures/Overflow.java \
			DataStructures/StackAr.java \
			DataStructures/StackArTester.java \
			DataStructures/Underflow.java
CLASS_FILES = $(subst .java,.class,${SRC_FILES})
TARGET_ARGS = DataStructures.StackArTester
DAIKON_ARGS = --no_text_output --no_show_progress --config config.txt --config_option daikon.Debug.show_stack_trace=true -o $@
CHICORY     = ${DAIKONDIR}/java/ChicoryPremain.jar
CHICORY_ARGS = --dtrace-file=$(basename $(basename $@)).dtrace
PREMAIN		= ${CHICORY}
DAIKON_CLASSPATH := ${DAIKONDIR}/java:${DAIKONDIR}/java/lib/*:${DAIKONDIR}/daikon.jar

TESTS		= online pptsel pptomit nesting stdvis varomit samples misc

# This is the working directory, but not necessarily the directory in
# which this Makefile is found.
HERE := $(shell /bin/pwd)

DAIKON_CLASSPATH := ${DAIKONDIR}/java:${DAIKONDIR}/java/lib/*:${DAIKONDIR}/daikon.jar

diffs : $(foreach x, ${TESTS}, $x.daikon.diff)

# obtain and compile the sources
compile : ${CLASS_FILES}
${CLASS_FILES} :
	tar cf - -C ${SRC_DIR} ${SRC_FILES} | tar xf - -C ${HERE}
	javac -g `find . -name *.java`

online.inv.gz : ${CLASS_FILES} ${CHICORY}
	java -cp .:${DAIKON_CLASSPATH} daikon.Chicory ${CHICORY_ARGS} \
	  --daikon-online --daikon-args '${DAIKON_ARGS}' ${TARGET_ARGS}

pptsel.inv.gz : ${CLASS_FILES} ${CHICORY}
	java -cp .:${DAIKON_CLASSPATH} daikon.Chicory ${CHICORY_ARGS} --ppt-select-pattern 'push|pop' \
	  --daikon --daikon-args '${DAIKON_ARGS}' ${TARGET_ARGS}

pptomit.inv.gz : ${CLASS_FILES} ${CHICORY}
	java -cp .:${DAIKON_CLASSPATH} daikon.Chicory ${CHICORY_ARGS} --ppt-omit-pattern 'push|pop' \
	  --daikon --daikon-args '${DAIKON_ARGS}' ${TARGET_ARGS}

nesting.inv.gz : ${CLASS_FILES} ${CHICORY}
	java -cp .:${DAIKON_CLASSPATH} daikon.Chicory ${CHICORY_ARGS} --nesting-depth 0 \
	  --daikon-online --daikon-args '${DAIKON_ARGS}' ${TARGET_ARGS}

stdvis.inv.gz : ${CLASS_FILES} ${CHICORY}
	java -cp .:${DAIKON_CLASSPATH} daikon.Chicory ${CHICORY_ARGS} --std-visibility \
	  --daikon --daikon-args '${DAIKON_ARGS}' ${TARGET_ARGS}

varomit.inv.gz : ${CLASS_FILES} ${CHICORY}
	java -cp .:${DAIKON_CLASSPATH} daikon.Chicory ${CHICORY_ARGS} --omit-var theArray \
	  --daikon --daikon-args '${DAIKON_ARGS}' ${TARGET_ARGS}

samples.inv.gz : ${CLASS_FILES} ${CHICORY}
	java -cp .:${DAIKON_CLASSPATH} daikon.Chicory ${CHICORY_ARGS} --sample-start=100 \
	  --daikon --daikon-args '${DAIKON_ARGS}' ${TARGET_ARGS}

misc.inv.gz : ${CLASS_FILES} ${CHICORY}
	java -cp .:${DAIKON_CLASSPATH} daikon.Chicory ${CHICORY_ARGS} --purity-file=StackAr.pure \
	--premain=${PREMAIN} --heap_size=100m \
	  --daikon --daikon-args '${DAIKON_ARGS}' ${TARGET_ARGS}

%.daikon.diff : %.inv.gz %.inv.txt.goal
	java -cp .:${DAIKON_CLASSPATH} daikon.PrintInvariants $< > $*.inv.txt
	-diff -w -u $*.inv.txt.goal $*.inv.txt > $@
	@cat $@

update-goals:
	$(foreach x, ${TESTS}, \
	  touch $x.inv.txt && cp $x.inv.txt $x.inv.txt.goal; )
	$(foreach x, ${TESTS}, rm -f $x.daikon.diff; )

clean:
	/bin/rm -rf *.inv.txt *.inv.gz *.diff *.dtrace *.dtrace.gz \
	${CLASS_FILES} ${SRC_FILES}

show-vars:
	@echo CLASS_FILES = ${CLASS_FILES}
