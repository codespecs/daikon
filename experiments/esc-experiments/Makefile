## Complete usage:
##     make reset-stackar-java compile data daikon merge esc

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
DAIKON_JAVA = $(INV)/daikon/Daikon.java

default:
	@echo "choose a target"

all: reset-stackar compile data daikon

reset-stackar-java:
	cd DataStructures; rm StackAr.java; ln -s StackAr.java-unannotated StackAr.java

# Do NOT run this lightly; doing so can throw the .decls and .dtrace files
# out of sync.  Also, probably run "make reset-stackar-java" first.
compile:
	dfej DataStructures/StackAr.java
	cd daikon-java && jikes DataStructures/*.java

esc: StackAr.esc

data: daikon-output/StackAr.dtrace

daikon: StackAr.txt-esc

merge:


###########################################################################

## All this really ought to be generalized into rules rather than being hard-coded.

daikon-output/StackAr.dtrace: daikon-java/DataStructures/StackArTester.java daikon-java/DataStructures/StackAr.java
	java -classpath daikon-java/:${CLASSPATH} DataStructures.StackArTester

StackAr.txt: daikon-output/StackAr.dtrace $(DAIKON_JAVA)
	java daikon.Daikon ${DAIKON_OPTS} $< daikon-output/DataStructures/*.decls >& StackAr.txt
	remove-conditional-ppts.pl StackAr.txt
#| perl -n -e 'print unless (/lexically/) || (/Variables/) || (/Samples breakdown/);' 

StackAr.txt-esc: daikon-output/StackAr.dtrace $(DAIKON_JAVA)
	java daikon.Daikon --esc_output ${DAIKON_OPTS} $< daikon-output/DataStructures/*.decls -o StackAr.inv >& StackAr.txt-esc
	remove-conditional-ppts.pl StackAr.txt-esc
#| perl -n -e 'print unless (/lexically/) || (/Variables/) || (/Samples breakdown/);' 

merge:
	cd DataStructures && merge-esc.pl ../StackAr.txt-esc
	rm -rf DataStructures/StackAr.java
	cd DataStructures && ln -s StackAr.java-escannotated StackAr.java

StackAr.esc: DataStructures/StackAr.java
	escjava DataStructures/StackAr.java > StackAr.esc

###########################################################################

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

###########################################################################
