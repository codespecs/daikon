## Complete usage:
##     make reset-stackar-java dfej jikes data daikon merge esc

DAIKON_OPTS=--suppress_cont

## Copied from another makefile; uncomment as needed
# DAIKON = java -mx128m daikon.Daikon
# DFEJ = dfej +E
# JAVAC := jikes -g +E +F
INV = $(HOME)/research/invariants
# TESTS = $(INV)/tests
# SCRIPTS = $(INV)/scripts
# MODBIT_MUNGE = $(SCRIPTS)/modbit-munge.pl
# TRACE_UNTRUNCATE = $(SCRIPTS)/trace-untruncate
TRACE_PURGE_FNS = $(SCRIPTS)/trace-purge-fns.pl
# TRACE_PURGE_VARS = $(SCRIPTS)/trace-purge-vars.pl
## In the normal case, all .class files under daikon/ have the same timestamp;
## Daikon.class can serve as a proxy for all of them.
DAIKON_CLASS = $(INV)/daikon/Daikon.class

default:
	@echo "choose a target"

all: reset-queuear-java compile data daikon

reset-queuear-java:
	cd DataStructures; rm QueueAr.java; ln -s QueueAr.java-unannotated QueueAr.java

# Do NOT run this lightly; doing so can throw the .decls and .dtrace files
# out of sync.  Also, probably run "make reset-queuear-java" first.
dfej:
	dfej DataStructures/QueueAr.java

jikes:
	cd daikon-java && jikes DataStructures/*.java

merge: merge-queuear

compile: dfej jikes

daikon: QueueAr.txt-esc

esc: QueueAr.esc

data: daikon-output/QueueAr.dtrace


###########################################################################

## All this really ought to be generalized into rules rather than being hard-coded.

daikon-output/StackAr.dtrace: daikon-java/DataStructures/StackArTester.java daikon-java/DataStructures/StackAr.java
	java -classpath daikon-java/:${CLASSPATH} DataStructures.StackArTester

StackAr.txt: daikon-output/StackAr.dtrace $(DAIKON_CLASS)
	java daikon.Daikon ${DAIKON_OPTS} $< daikon-output/DataStructures/*.decls >& StackAr.txt
	remove-conditional-ppts.pl StackAr.txt
#| perl -n -e 'print unless (/lexically/) || (/Variables/) || (/Samples breakdown/);' 

StackAr.txt-esc: daikon-output/StackAr.dtrace $(DAIKON_CLASS)
	java daikon.Daikon --esc_output ${DAIKON_OPTS} $< daikon-output/DataStructures/*.decls -o StackAr.inv >& StackAr.txt-esc
	remove-conditional-ppts.pl StackAr.txt-esc
#| perl -n -e 'print unless (/lexically/) || (/Variables/) || (/Samples breakdown/);' 

merge-stackar:
	cd DataStructures && merge-esc.pl ../StackAr.txt-esc
	rm -rf DataStructures/StackAr.java
	cd DataStructures && ln -s StackAr.java-escannotated StackAr.java

StackAr.esc: DataStructures/StackAr.java
	escjava DataStructures/StackAr.java > StackAr.esc

###########################################################################

daikon-output/QueueAr.dtrace: daikon-java/DataStructures/QueueArTester.java daikon-java/DataStructures/QueueAr.java
	java -classpath daikon-java/:${CLASSPATH} DataStructures.QueueArTester

QueueAr.txt: daikon-output/QueueAr.dtrace $(DAIKON_CLASS)
	java daikon.Daikon ${DAIKON_OPTS} $< daikon-output/DataStructures/*.decls >& QueueAr.txt
	remove-conditional-ppts.pl QueueAr.txt
#| perl -n -e 'print unless (/lexically/) || (/Variables/) || (/Samples breakdown/);' 

QueueAr.txt-esc: daikon-output/QueueAr.dtrace $(DAIKON_CLASS)
	java daikon.Daikon --esc_output ${DAIKON_OPTS} $< daikon-output/DataStructures/*.decls -o QueueAr.inv >& QueueAr.txt-esc
	remove-conditional-ppts.pl QueueAr.txt-esc
#| perl -n -e 'print unless (/lexically/) || (/Variables/) || (/Samples breakdown/);' 

merge-queuear:
	cd DataStructures && merge-esc.pl ../QueueAr.txt-esc
	rm -rf DataStructures/QueueAr.java
	cd DataStructures && ln -s QueueAr.java-escannotated QueueAr.java

QueueAr.esc: DataStructures/QueueAr.java
	escjava DataStructures/QueueAr.java > QueueAr.esc

###########################################################################

## Extras that should be patterned after the above once it is generalized.

daikon-output/StackAr-init.dtrace: daikon-output/StackAr.dtrace
	cp -pf $< $@
	trace-purge-fns.pl -v '<init>' $@

daikon-init: daikon-output/StackAr-init.dtrace
	java daikon.Daikon ${DAIKON_OPTS} $< daikon-output/DataStructures/*.decls -o StackAr-init.inv > StackAr-init.txt

daikon-output/StackAr-makeEmpty.dtrace: daikon-output/StackAr.dtrace
	cp -pf $< $@
	trace-purge-fns.pl -v 'makeEmpty' $@

daikon-makeempty: daikon-output/StackAr-makeEmpty.dtrace
	java daikon.Daikon ${DAIKON_OPTS} $< daikon-output/DataStructures/*.decls -o StackAr-init.inv > StackAr-makeEmpty.txt

daikon-output/StackAr-push.dtrace: daikon-output/StackAr.dtrace
	cp -pf $< $@
	trace-purge-fns.pl -v 'push' $@

daikon-push: daikon-output/StackAr-push.dtrace
	java daikon.Daikon ${DAIKON_OPTS} $< daikon-output/DataStructures/*.decls -o StackAr-init.inv > StackAr-push.txt

daikon-output/QueueAr-dequeue.dtrace: daikon-output/QueueAr.dtrace
	cp -pf $< $@
	trace-purge-fns.pl -v 'dequeue' $@

daikon-dequeue: daikon-output/QueueAr-dequeue.dtrace
	java daikon.Daikon ${DAIKON_OPTS} $< daikon-output/DataStructures/*.decls -o QueueAr-init.inv > QueueAr-dequeue.txt

daikon-dequeue-esc: daikon-output/QueueAr-dequeue.dtrace
	java daikon.Daikon --esc_output ${DAIKON_OPTS} $< daikon-output/DataStructures/*.decls -o QueueAr-init.inv > QueueAr-dequeue.txt-esc

###########################################################################
