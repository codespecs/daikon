### User customizations

# Put user-specific changes (such as changes to the JAVAC variable)
#   in your own Makefile.user file in this directory.
# Make will silently continue if Makefile.user does not exist.
-include Makefile.user

# JAVAC ?= jikes -g +E +F
JAVAC ?= javac -g

### end of user customizations


###########################################################################
### Variables
###

# This command should run after the AUTO_GENERATED_FILES are generated, or
# else AUTO_GENERATED_FILES should be appended to it (without duplication).
# In practice, this is good enough, however.
JAVA_FILES := $(shell find . -follow \( -name '*daikon-java*' -o -name '*daikon-instrumented*' -o -name CVS -o -path './jakarta-oro*/src/java/examples/awk' -o -name 'ReturnBytecodes.java' -o -name 'AjaxDecls.java' -o -name 'ajax-ship' \) -prune -o -name '*.java' -print)
TAG_FILES := $(shell find . -follow \( -name '*daikon-java*' -o -name CVS -o -name 'split' \) -prune -o \( -name '*.java' -o -name '*.java.jpp' \) -print)

AUTO_GENERATED_FILES = \
	utilMDE/MathMDE.java \
	daikon/PptSlice1.java \
	daikon/PptSlice2.java \
	daikon/PptSlice3.java \
	daikon/inv/unary/LowerBoundCore.java \
	daikon/inv/unary/UpperBoundCore.java \
	daikon/inv/unary/scalar/LowerBound.java \
	daikon/inv/unary/scalar/UpperBound.java \
	daikon/inv/unary/scalar/OneOfScalar.java \
	daikon/inv/unary/string/OneOfString.java \
	daikon/inv/unary/sequence/OneOfSequence.java \
	daikon/inv/unary/sequence/EltOneOf.java \
	daikon/inv/unary/sequence/EltLowerBound.java \
	daikon/inv/unary/sequence/EltUpperBound.java \
	daikon/inv/unary/stringsequence/EltOneOfString.java \
	daikon/inv/unary/stringsequence/OneOfStringSequence.java \
	daikon/inv/binary/sequenceScalar/Member.java \
	daikon/inv/binary/sequenceString/Member.java \
	daikon/inv/binary/twoScalar/IntEqual.java \
	daikon/inv/binary/twoScalar/IntNonEqual.java \
	daikon/inv/binary/twoScalar/IntLessThan.java \
	daikon/inv/binary/twoScalar/IntLessEqual.java \
	daikon/inv/binary/twoScalar/IntGreaterThan.java \
	daikon/inv/binary/twoScalar/IntGreaterEqual.java \
	daikon/derive/binary/SequenceScalarSubscript.java \
	daikon/derive/binary/SequenceStringSubscript.java \
	daikon/derive/binary/SequenceScalarSubsequence.java \
	daikon/derive/binary/SequenceStringSubsequence.java \
	daikon/derive/binary/SequenceScalarSubscriptFactory.java \
	daikon/derive/binary/SequenceStringSubscriptFactory.java \
	daikon/derive/binary/ScalarSequencesIntersection.java \
	daikon/derive/binary/StringSequencesIntersection.java \
	daikon/derive/binary/ScalarSequencesIntersectionFactory.java \
	daikon/derive/binary/StringSequencesIntersectionFactory.java \
	daikon/derive/binary/ScalarSequencesUnion.java \
	daikon/derive/binary/StringSequencesUnion.java \
	daikon/derive/binary/ScalarSequencesUnionFactory.java \
	daikon/derive/binary/StringSequencesUnionFactory.java \

###########################################################################
### Compilation targets
###

default: all

all:	all_directly junit

## In general, don't use this; it misses some files
all_via_daikon: Daikon.class Runtime.class Diff.class

## ${JAVA_FILES} is so long I can't see the result on the same screen as the command.
all_directly: ${AUTO_GENERATED_FILES} ajax
	@echo ${JAVAC} '*.java ...'
	@${JAVAC} ${JAVA_FILES}
	@chmod -w ${AUTO_GENERATED_FILES}

## Use this when you don't want $inv/tests makefiles to restart
JAVA_FILES_EXCEPT_DAIKON := $(subst ./daikon/Daikon.java,,${JAVA_FILES})
all_except_daikon: ${JAVA_FILES_EXCEPT_DAIKON}
	@echo ${JAVAC} '*.java ... (except Daikon.java)'
	@${JAVAC} ${JAVA_FILES_EXCEPT_DAIKON}
	@chmod -w ${AUTO_GENERATED_FILES}

all_force:	all_directly

all_via_javac:
	$(MAKE) JAVAC='javac -g' all

all_javac:	all_via_javac

javac:	all_via_javac

all_via_jikes:
	$(MAKE) JAVAC='jikes -g +E +F' all

all_jikes:	all_via_jikes

jikes:	all_via_jikes

# These rules are undesirable, because Daikon.class (or Diff.class) might
# be up to date even if other files aren't.
daikon/Daikon.class: daikon/Daikon.java	${AUTO_GENERATED_FILES}
	${JAVAC} daikon/Daikon.java

daikon/Diff.class: Diff.java ${AUTO_GENERATED_FILES}
	${JAVAC} daikon/Diff.java

daikon/Runtime.class: daikon/Runtime.java
	${JAVAC} daikon/Runtime.java

ajax:
	ln -s ajax-ship/ajax .


###########################################################################
### Non-compilation targets
###

# It's good to run "make clean" occasionally, because it costs little to
# regenerate .class files and it's bad to continue to use an orphaned class
# file (whose source file was renamed).  Or, run "orphaned-class-files".
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
	@echo etags '*.java ...'
	@etags -l java ${TAG_FILES}

## Static checks of the code

lint:	jlint

jlint:
	jlint . | grep -v "Value of referenced variable '.*' may be NULL" | grep -v "Compare strings as object references" | perl -p -e 's:^daikon/::'

# "(cd $inv; maudit daikon)" does not work; I don't understand why.
# Unfortunately, this is way too much output; I need to filter it.
# Maybe write my own script to do that...
audit:
	cd $(HOME)/java && maudit daikon | perl -p -e 's/^daikon\///'

## Testing

test: tests

tests: junit
	cd ../tests && $(MAKE) all

# run a static check on currently-compiled classes
# ensure serialized classes have a date-style UID assigned
check-serialize:
#	for each non-gui class, run serialver
#	accept only "is not Serializable" or "UID = 200#####" as output
	@find daikon -follow -name 'gui' -prune -o -name '*.class' -print | perl -ne 's|\.class\n$$||; s|/|.|g; print "Checking $$_... \t"; $$_ =~ s|\$$|\\\$$|g; $$msg =`serialver $$_ 2>&1`; if ($$msg =~ m/is not Serializable/) { print "ok (not serialized)\n"; next; } if ($$msg =~m/UID = (200\d[01]\d[0123]\d)L;/) { print "ok ($$1)\n"; next; }  print "WARNING: incorrect: $$msg";'


.PHONY: junit
# Include CLASSPATH on command line for Cygwin compatibility
junit:
	@echo java daikon.test.MasterTester
	@java -classpath ${CLASSPATH} daikon.test.MasterTester

## Documentation

.PHONY : doc

JAVADOC_FLAGS = -d doc \
                -linkoffline http://java.sun.com/j2se/1.3/docs/api/ http://java.sun.com/j2se/1.3/docs/api/
JAVADOC_PACKAGES = `find -follow -name '*.java' -exec dirname {} \; | grep -v CVS | grep -v ajax | sort | uniq | perl -p -e 's:^./::g' | perl -p -e 's:/:.:g'`

doc: $(JAVA_FILES)
	mkdir -p doc
	javadoc $(JAVADOC_FLAGS) $(JAVADOC_PACKAGES)

## Make doc with private fields for people working on source code
doc-all: $(JAVA_FILES)
	mkdir -p doc
	javadoc $(JAVADOC_FLAGS) -private $(JAVADOC_PACKAGES)

doc-jdk-lookup: doc
	jdk-add-munged-anchor-names-no-bak `find doc -name '*.html'`
	echo "Run jdk-index-to-alist > ~/emacs/jdk-index.el"


## For debugging the Makefile.
showvars:
	@echo JAVA_FILES = $(JAVA_FILES)
	@echo TAG_FILES = $(TAG_FILES)

list_java_files:
	@echo $(JAVA_FILES)


###########################################################################
### Automatically generated files
###

# Create the auto-generated files with jpp
.PHONY: jpp
jpp: ${AUTO_GENERATED_FILES}

# Use this rule if you do not have cpp or for some other reason do not want
# to refresh the automatically generated .java files.  You should not do
# this if those files are not up-to-date (that is, if you have modified the
# .jpp files).  It is safe to do this with a distrubution that you have
# just unpacked.
avoid-jpp:
	@echo "Any recent changes in the .jpp files will not get propagated."
	touch ${AUTO_GENERATED_FILES}


# ":=" does not work here; the variables get expanded too soon.
# "-e" means exit immediately if any error, rather than returning 
# the status of the last command.

# This rule does not work with make 3.77, but does work with make 3.78.
JAVA_CPP ?= (rm -f $@; java-cpp.pl -DDEFINEDVAR $< 2>&1 > $@ | grep Can && (rm -f $@ && false) || chmod -w $@)
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

utilMDE/MathMDE.java: utilMDE/MathMDE.java.jpp utilMDE/MathMDE-gcd.java.jpp utilMDE/MathMDE-modulus.java.jpp utilMDE/MathMDE-nonmodulus.java.jpp utilMDE/MathMDE-help.java.jpp utilMDE/MathMDE-helpend.java.jpp
	cd utilMDE && $(MAKE) MathMDE.java

daikon/PptSlice1.java: daikon/PptSlice.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ARITY1)

daikon/PptSlice2.java: daikon/PptSlice.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ARITY2)

daikon/PptSlice3.java: daikon/PptSlice.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ARITY3)

daikon/inv/unary/LowerBoundCore.java: daikon/inv/unary/BoundCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LOWER)

daikon/inv/unary/UpperBoundCore.java: daikon/inv/unary/BoundCore.java.jpp
	$(JAVA_CPP:DEFINEDVAR=UPPER)

daikon/inv/unary/scalar/LowerBound.java: daikon/inv/unary/scalar/Bound.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LOWER)

daikon/inv/unary/scalar/UpperBound.java: daikon/inv/unary/scalar/Bound.java.jpp
	$(JAVA_CPP:DEFINEDVAR=UPPER)

daikon/inv/unary/scalar/OneOfScalar.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/inv/unary/string/OneOfString.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/inv/unary/sequence/OneOfSequence.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SEQUENCE)

daikon/inv/unary/sequence/EltOneOf.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ELT)

daikon/inv/unary/sequence/EltLowerBound.java: daikon/inv/unary/scalar/Bound.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ELTLOWER)

daikon/inv/unary/sequence/EltUpperBound.java: daikon/inv/unary/scalar/Bound.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ELTUPPER)

daikon/inv/unary/stringsequence/EltOneOfString.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=ELTSTRING)

daikon/inv/unary/stringsequence/OneOfStringSequence.java: daikon/inv/unary/OneOf.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRINGSEQUENCE)

daikon/inv/binary/sequenceScalar/Member.java: daikon/inv/binary/Member.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/inv/binary/sequenceString/Member.java: daikon/inv/binary/Member.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/inv/binary/twoScalar/IntEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=EQUAL)

daikon/inv/binary/twoScalar/IntNonEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=NONEQUAL)

daikon/inv/binary/twoScalar/IntLessThan.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSTHAN)

daikon/inv/binary/twoScalar/IntLessEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=LESSEQUAL)

daikon/inv/binary/twoScalar/IntGreaterThan.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATERTHAN)

daikon/inv/binary/twoScalar/IntGreaterEqual.java: daikon/inv/binary/twoScalar/IntComparisons.java.jpp
	$(JAVA_CPP:DEFINEDVAR=GREATEREQUAL)

daikon/derive/binary/SequenceScalarSubscript.java: daikon/derive/binary/SequenceSubscript.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/SequenceStringSubscript.java: daikon/derive/binary/SequenceSubscript.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/SequenceScalarSubsequence.java: daikon/derive/binary/SequenceSubsequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/SequenceStringSubsequence.java: daikon/derive/binary/SequenceSubsequence.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/SequenceScalarSubscriptFactory.java: daikon/derive/binary/SequenceSubscriptFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/SequenceStringSubscriptFactory.java: daikon/derive/binary/SequenceSubscriptFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/ScalarSequencesIntersection.java: daikon/derive/binary/SequencesIntersection.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/StringSequencesIntersection.java: daikon/derive/binary/SequencesIntersection.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/ScalarSequencesIntersectionFactory.java: daikon/derive/binary/SequencesIntersectionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/StringSequencesIntersectionFactory.java: daikon/derive/binary/SequencesIntersectionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/ScalarSequencesUnion.java: daikon/derive/binary/SequencesUnion.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/StringSequencesUnion.java: daikon/derive/binary/SequencesUnion.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)

daikon/derive/binary/ScalarSequencesUnionFactory.java: daikon/derive/binary/SequencesUnionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=SCALAR)

daikon/derive/binary/StringSequencesUnionFactory.java: daikon/derive/binary/SequencesUnionFactory.java.jpp
	$(JAVA_CPP:DEFINEDVAR=STRING)


## I decided against this.  It gave little enough help, and it cluttered
## the directories enough (and could be slightly confusing about what those
## new types really do), that I will hold off on this for now.  Perhaps it
## would be more reasonable if all of the specialized files were in one
## Vector subdirectory (that was ignored by tags, etc.).

# This one is just for testing
IntegerVector.java: VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon -DELTVECTOR=IntegerVector -DELTTYPE=Integer")

daikon/PptVector.java: daikon/VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon -DELTVECTOR=PptVector -DELTTYPE=Ppt")

daikon/PptTopLevelVector.java: daikon/VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon -DELTVECTOR=PptTopLevelVector -DELTTYPE=PptTopLevel")

daikon/PptConditionalVector.java: daikon/VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon -DELTVECTOR=PptConditionalVector -DELTTYPE=PptConditional")

daikon/VarInfoVector.java: daikon/VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon -DELTVECTOR=VarInfoVector -DELTTYPE=VarInfo")

daikon/inv/InvariantVector.java: daikon/VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon.inv -DELTVECTOR=InvariantVector -DELTTYPE=Invariant")

daikon/derive/DerivationVector.java: daikon/ VectorTemplate.java.jpp
	$(JAVA_CPP:DEFINEDVAR="PACKAGE=daikon.derive -DELTVECTOR=DerivationVector -DELTTYPE=Derivation")


###########################################################################
### Delete-on-error
###

# Existence of this rule means that a target is deleted if has just changed
# and its rule commands exit with nonzero status.  It's needed for the
# java-cpp rules above.
# (Actually, it isn't anymore, for I arrange not to overwrite the file
# unless success.  But leave it in for now anyway.)
.DELETE_ON_ERROR:
