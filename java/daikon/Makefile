### User customizations

# Put user-specific changes in your own Makefile.user
#  (such as changes to UTILMDE_DIR or JAVAC).
# Make will silently continue if file does not exist.
-include Makefile.user

UTILMDE_DIR ?= $(HOME)/java/utilMDE
JAVAC ?= jikes -g +E +F

### end of user customications


JAVA_FILES := $(shell find . \( -name '*daikon-java*' -o -name '*-cpp.java' -o -name CVS -o -name 'ReturnBytecodes.java' -o -name 'AjaxDecls.java' \) -prune -o -name '*.java' -print)
TAG_FILES := $(shell find . \( -name '*daikon-java*' -o -name CVS -o -name 'split' \) -prune -o -name '*.java' -print)
# Should these really be targets in the below, or should I use "make
# ${AUTO_GENERATED_FILES}"? I don't want to omit running just because these
# are up-to-date.
AUTO_GENERATED_FILES = \
	PptSlice1.java \
	PptSlice2.java \
	PptSlice3.java \
	inv/unary/scalar/LowerBound.java \
	inv/unary/scalar/UpperBound.java \
	inv/unary/scalar/OneOfScalar.java \
	inv/unary/string/OneOfString.java \
	inv/unary/sequence/OneOfSequence.java \
	inv/unary/sequence/EltOneOf.java \
	inv/unary/stringsequence/EltOneOfString.java \
	inv/unary/stringsequence/OneOfStringSequence.java \
	inv/binary/sequenceScalar/Member.java \
	inv/binary/sequenceString/Member.java \
	derive/binary/SequenceScalarSubscript.java \
	derive/binary/SequenceStringSubscript.java \
	derive/binary/SequenceScalarSubsequence.java \
	derive/binary/SequenceStringSubsequence.java \
	derive/binary/SequenceScalarSubscriptFactory.java \
	derive/binary/SequenceStringSubscriptFactory.java \


all:	all_directly

## In general, don't use this; it misses some files
all_via_daikon:	Runtime.class Diff.class ${AUTO_GENERATED_FILES}
	${JAVAC} Daikon.java

## ${JAVA_FILES} is so long I can't see the result on the same screen as the command.
all_directly: ${AUTO_GENERATED_FILES}
	@echo ${JAVAC} '*.java ...'
	@${JAVAC} ${JAVA_FILES}

all_force:	all_directly

all_via_javac:
	$(MAKE) JAVAC='javac -g' all

all_javac:	all_via_javac

Runtime.class: Runtime.java
	${JAVAC} Runtime.java

Diff.class: Diff.java ${AUTO_GENERATED_FILES}
	${JAVAC} Diff.java

# Wrong, becaue Daikon.class might be up to date even if other files aren't
# Daikon.class:
# 	${JAVAC} Daikon.java

# It's good to do this occasionally, because it costs little to regenerate
# .class files and it's bad to continue to use an orphaned class file
# (whose source file was renamed).  Or, run "orphaned-class-files".
clean:
	-rm -f `find . -name "*.class" -print`

dist_clean: clean
	-rm -f TAGS

very_clean: dist_clean
	-rm -f ${AUTO_GENERATED_FILES}

run:	all
	java daikon.Daikon

run_gui:	all
	java daikon.gui.InvariantsGUI /g1/users/mhao/daikon/inv_files/dsaa.inv 2>&1 | grep -v Font

TAGS:	tags

# Used to include "*.py", but no such in daikon/ directory.
# I could make this ../*.py, but probably don't want to include that old code.
tags:
	etags ${TAG_FILES} $(UTILMDE_DIR)/*.java

doc: $(JAVA_FILES)
	javadoc -d doc `find -name '*.java' -exec dirname {} \; | sort | uniq | perl -p -e 's/^./daikon/g' | perl -p -e 's:/:.:g'`
	jdk-add-munged-anchor-names doc/daikon/*.html
	@echo "Run jdk-index-to-alist > ~/emacs/jdk-index.el"

lint:	jlint

jlint:
	jlint . | grep -v "Value of referenced variable '.*' may be NULL" | grep -v "Compare strings as object references" | perl -p -e 's:^daikon/::'

# "(cd $inv; maudit daikon)" does not work; I don't understand why.
# Unfortunately, this is way too much output; I need to filter it.
# Maybe write my own script to do that...
audit:
	cd $(HOME)/java && maudit daikon | perl -p -e 's/^daikon\///'

test: tests

tests:
	cd ../tests && $(MAKE) all

showvars:
	@echo JAVA_FILES = $(JAVA_FILES)

list_java_files:
	@echo $(JAVA_FILES)


###########################################################################

## Automatically generated files


# ":=" does not work here; the variables get expanded too soon.
# "-e" means exit immediately if any error, rather than returning 
# the status of the last command.

JAVA_CPP = (rm -f $@; set -e; java-cpp.pl -DDEFINEDVAR $< >| $@; chmod -w $@)
# Like the above, but also makes a backup file
# JAVA_CPP = (mv -f $@ $@-bak; set -e; java-cpp.pl -DDEFINEDVAR $< > $@; chmod -w $@)

# JAVA_CPP = (cp -pf $@ $@-bak; set -e; java-cpp $< -DDEFINEDVAR > $@-postproc; mv -f $@-postproc $@; chmod -w $@)
## Here are alternate ways of writing the same command:
## I could use "define", but that's specific to GNU Make
## I can't do this because the CPP defined variable changes.
# JAVA_CPP = (mv -f $@ $@-bak; set -e; java-cpp $< -DDEFINEDVAR > $@; chmod -w $@)
## These are used like:	$(JAVA_CPP_START) -DLOWER $(JAVA_CPP_END)
# JAVA_CPP_START = (cp -pf $@ $@-bak; set -e; java-cpp $<
# JAVA_CPP_END = > $@-postproc; mv -f $@-postproc $@; chmod -w $@)


PptSlice1.java: PptSlice-cpp.java
	$(JAVA_CPP:DEFINEDVAR=ARITY1)

PptSlice2.java: PptSlice-cpp.java
	$(JAVA_CPP:DEFINEDVAR=ARITY2)

PptSlice3.java: PptSlice-cpp.java
	$(JAVA_CPP:DEFINEDVAR=ARITY3)

inv/unary/scalar/LowerBound.java: inv/unary/scalar/Bound-cpp.java
	$(JAVA_CPP:DEFINEDVAR=LOWER)

inv/unary/scalar/UpperBound.java: inv/unary/scalar/Bound-cpp.java
	$(JAVA_CPP:DEFINEDVAR=UPPER)

inv/unary/scalar/OneOfScalar.java: inv/unary/OneOf-cpp.java
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

inv/unary/string/OneOfString.java: inv/unary/OneOf-cpp.java
	$(JAVA_CPP:DEFINEDVAR=STRING)

inv/unary/sequence/OneOfSequence.java: inv/unary/OneOf-cpp.java
	$(JAVA_CPP:DEFINEDVAR=SEQUENCE)

inv/unary/sequence/EltOneOf.java: inv/unary/OneOf-cpp.java
	$(JAVA_CPP:DEFINEDVAR=ELT)

inv/unary/stringsequence/EltOneOfString.java: inv/unary/OneOf-cpp.java
	$(JAVA_CPP:DEFINEDVAR=ELTSTRING)

inv/unary/stringsequence/OneOfStringSequence.java: inv/unary/OneOf-cpp.java
	$(JAVA_CPP:DEFINEDVAR=STRINGSEQUENCE)

inv/binary/sequenceScalar/Member.java: inv/binary/Member-cpp.java
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

inv/binary/sequenceString/Member.java: inv/binary/Member-cpp.java
	$(JAVA_CPP:DEFINEDVAR=STRING)

derive/binary/SequenceScalarSubscript.java: derive/binary/SequenceSubscript-cpp.java
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

derive/binary/SequenceStringSubscript.java: derive/binary/SequenceSubscript-cpp.java
	$(JAVA_CPP:DEFINEDVAR=STRING)

derive/binary/SequenceScalarSubsequence.java: derive/binary/SequenceSubsequence-cpp.java
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

derive/binary/SequenceStringSubsequence.java: derive/binary/SequenceSubsequence-cpp.java
	$(JAVA_CPP:DEFINEDVAR=STRING)

derive/binary/SequenceScalarSubscriptFactory.java: derive/binary/SequenceSubscriptFactory-cpp.java
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

derive/binary/SequenceStringSubscriptFactory.java: derive/binary/SequenceSubscriptFactory-cpp.java
	$(JAVA_CPP:DEFINEDVAR=STRING)


###########################################################################
###
###

# Existence of this rule means that a target is deleted if has just changed
# and its rule commands exit with nonzero status.  It's needed for the
# java-cpp rules above.
# (Actually, it isn't anymore, for I arrange not to overwrite the file
# unless success.  But leave it in for now anyway.)

.DELETE_ON_ERROR:
